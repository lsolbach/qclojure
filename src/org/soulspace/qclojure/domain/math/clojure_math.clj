;; Rebalanced full content
 (ns org.soulspace.qclojure.domain.math.clojure-math
   "Pure Clojure math backend implementing the domain math protocols using
  persistent vectors and simple maps. Complex scalars, vectors and matrices
  use a Split-of-Arrays (SoA) representation with :real / :imag keys.

  This backend prioritizes clarity and correctness; performance optimizations
  (transients, unchecked math, primitive arrays) can be layered later.

  Advanced decompositions are mostly placeholders; delegate heavy numerics to
  specialized backends (e.g. Neanderthal) for production performance."
   (:require [org.soulspace.qclojure.domain.math.protocols :as proto]
             [fastmath.complex :as fc]))

;;
;; Configuration / defaults
;;
(def ^:const ^double default-tolerance 1.0e-12)

(defn tolerance* [backend]
  (double (or (:tolerance backend) (:tolerance (:config backend)) default-tolerance)))

;;
;; Predicates & helpers for complex canonical forms
;;
(defn complex-scalar? [x]
  (and (map? x) (contains? x :real) (contains? x :imag) (number? (:real x)) (number? (:imag x))))

(defn complex-vector? [v]
  (and (map? v) (contains? v :real) (contains? v :imag)
       (vector? (:real v)) (vector? (:imag v)) (= (count (:real v)) (count (:imag v)))))

(defn complex-matrix? [m]
  (and (map? m) (contains? m :real) (contains? m :imag)
       (vector? (:real m)) (vector? (:imag m))
       (= (count (:real m)) (count (:imag m)))
       (every? vector? (:real m)) (every? vector? (:imag m))
       (= (map count (:real m)) (map count (:imag m)))))

(defn make-complex [r i] {:real (double r) :imag (double i)})

(defn ensure-complex-scalar [x]
  (cond (complex-scalar? x) x
        (number? x) (make-complex x 0.0)
        :else (throw (ex-info "Unsupported scalar for complex coercion" {:value x}))))

(defn ensure-complex-vector [v]
  (cond (complex-vector? v) v
        (vector? v) {:real (mapv double v) :imag (mapv (constantly 0.0) v)}
        :else (throw (ex-info "Unsupported vector for complex coercion" {:value v}))))

(defn ensure-complex-matrix [m]
  (cond (complex-matrix? m) m
        (vector? m) {:real (mapv (fn [row] (mapv double row)) m)
                     :imag (mapv (fn [row] (mapv (constantly 0.0) row)) m)}
        :else (throw (ex-info "Unsupported matrix for complex coercion" {:value m}))))

;;
;; Low-level numeric helpers
;;
(defn- identity-matrix [n]
  (vec (for [i (range n)] (vec (for [j (range n)] (double (if (= i j) 1.0 0.0)))))))

(defn- matrix-shape [A]
  (if (complex-matrix? A)
    [(count (:real A)) (count (first (:real A)))]
    [(count A) (count (first A))]))

;; (Potential helpers zeros/zero-matrix/same-shape? removed to avoid warnings; reintroduce when needed.)

;;
;; Real matrix operations
;;
(defn- real-add [A B]
  (mapv (fn [ra rb] (mapv #(+ (double %1) (double %2)) ra rb)) A B))

(defn- real-sub [A B]
  (mapv (fn [ra rb] (mapv #(- (double %1) (double %2)) ra rb)) A B))

(defn- real-scale [A a]
  (let [a (double a)] (mapv (fn [row] (mapv #(* a (double %)) row)) A)))

(defn- real-mul [A B]
  (let [m (count A) n (count (first B))
        bt (apply map vector B)]
    (vec (for [i (range m)]
           (vec (for [j (range n)]
                  (reduce + (map * (nth A i) (nth bt j)))))))))

;; ---------------------------------------------------------------------------
;; Forward / Back substitution (real) for unit-lower L and upper U
;; Used by pivoted solve/inverse delegating to existing LU decomposition.
;; ---------------------------------------------------------------------------

(defn- real-forward-sub
  "Solve L y = b for y where L is unit lower triangular (diagonal = 1).

  Parameters:
  - L: n x n unit lower triangular matrix (vector of row vectors)
  - b: right hand side vector length n

  Returns: vector y.
  Complexity: O(n^2)."
  [L b]
  (let [n (count L)]
    (loop [i 0 y (transient (vec (repeat n 0.0)))]
      (if (= i n)
        (persistent! y)
        (let [sum (reduce + (map #(* (get-in L [i %]) (nth y %)) (range i)))
              yi (- (double (nth b i)) sum)]
          (recur (inc i) (assoc! y i yi)))))))

(defn- real-back-sub
  "Solve U x = y for x where U is upper triangular.

  Parameters:
  - U: n x n upper triangular matrix
  - y: RHS vector length n

  Returns: solution vector x.
  Complexity: O(n^2)."
  [U y]
  (let [n (count U)]
    (loop [i (dec n) x (transient (vec (repeat n 0.0)))]
      (if (neg? i)
        (persistent! x)
        (let [row (nth U i)
              diag (double (nth row i))
              _ (when (zero? diag) (throw (ex-info "Singular matrix in back substitution" {:i i})))
              sum (reduce + (map (fn [j] (* (double (nth row j)) (nth x j))) (range (inc i) n)))
              xi (/ (- (double (nth y i)) sum) diag)]
          (recur (dec i) (assoc! x i xi)))))))

;; ---------------------------------------------------------------------------
;; Householder QR (real matrices)
;; Provides numerically stable orthogonal-triangular decomposition.
;; ---------------------------------------------------------------------------

(defn- householder-qr-real
  "Compute QR decomposition of real matrix A via Householder reflectors.

  Parameters:
  - A: m x n real matrix (vector of row vectors)

  Returns map {:Q Q :R R}
  where Q is m x m orthogonal, R is m x n upper-triangular (below-diagonal
  entries explicitly zeroed for clarity). This implementation favors
  clarity; performance optimizations (blocking, packed storage) can be
  layered later.

  Algorithm outline per Golub & Van Loan:
  For k = 0 .. min(m-1,n-1):
    x = A[k:m, k]
    v = x + sign(x0)*||x||*e1
    v = v / ||v||
    A[k:m, k:n] -= 2 v (v^T A[k:m, k:n])
    Accumulate Q via right-multiplication by reflector (or build explicitly).

  Complexity: O(m n^2) general case.
  Stability: Backward stable; avoids loss of orthogonality inherent in
  classical Gram–Schmidt used previously."
  [A]
  (let [[m n] (matrix-shape A)
        rmin (min m n)
        A0 (mapv vec A)
        ;; Mutable (persistent) working copy of A accumulated into R
        identity-m (identity-matrix m)]
    (loop [k 0 A* A0 Q identity-m]
      (if (= k rmin)
        (let [R (->> A*
                     (map-indexed (fn [i row]
                                    (map-indexed (fn [j v] (if (< j i) 0.0 v)) row)))
                     (mapv vec))]
          {:Q Q :R R})
        (let [;; Extract x = A[k:m, k]
              x (mapv #(get-in A* [% k]) (range k m))
              normx (Math/sqrt (reduce + (map #(* (double %) (double %)) x)))
              sign (if (or (zero? normx) (neg? (double (first x)))) -1.0 1.0)
              alpha (* sign normx)
              u1 (+ (double (first x)) alpha)
              v (if (zero? normx)
                  (vec (cons 1.0 (repeat (dec (count x)) 0.0)))
                  (let [vraw (cons u1 (rest x))
                        vnorm (Math/sqrt (reduce + (map #(* % %) vraw)))
                        vnorm (if (pos? vnorm) vnorm 1.0)]
                    (mapv #(/ (double %) vnorm) vraw)))
              beta 2.0
              ;; Apply reflector H = I - beta v v^T to submatrix A[k:m, k:n]
              A** (reduce (fn [A** j]
                            (let [col (mapv #(get-in A** [% j]) (range k m))
                                  dot (reduce + (map * v col))
                                  ;; Householder: H = I - 2 v v^T (v normalized) => col' = col - 2 (v^T col) v
                                  scaled (* beta dot) ; beta = 2
                                  newcol (mapv (fn [vi ci] (- ci (* scaled vi))) v col)]
                              (reduce (fn [A*** idx]
                                        (assoc-in A*** [(+ k idx) j] (nth newcol idx)))
                                      A** (range (count v)))))
                          A* (range k n))
              ;; Update Q ← Q H^T = Q (I - beta v v^T) since H symmetric
              Q* (reduce (fn [Q* i]
                           (let [row (get Q* i)
                                 segment (subvec row k m)
                                 dot (reduce + (map * segment v))
                                 scaled (* beta dot)
                                 newseg (mapv (fn [si vi] (- si (* scaled vi))) segment v)]
                             (assoc Q* i (vec (concat (subvec row 0 k) newseg (subvec row m))))))
                         Q (range m))]
          (recur (inc k) A** Q*))))))

(defn- real-transpose [A] (vec (apply mapv vector A)))

(defn- real-frobenius-norm [A]
  (Math/sqrt (reduce + (for [row A v row] (let [d (double v)] (* d d))))))

(defn- real-one-norm [A]
  (apply max (map (fn [col] (reduce + (map #(Math/abs (double %)) col))) (apply map vector A))))

;; Jacobi eigen-decomposition (shared helper)
;;
;; The routine is intentionally simple (no pivot strategies beyond largest
;; off-diagonal, no blocking) and targets small/medium matrices typical for
;; algorithmic construction and test cases. Heavy-duty performance should be
;; delegated to a native/optimized backend.

(defn- jacobi-symmetric
  "Compute eigen-decomposition of a real symmetric matrix via classical
  Jacobi rotations.

  Parameters:
  - A      real symmetric square matrix (vector of row vectors)
  - tol    convergence tolerance on largest off-diagonal absolute value
  - max-it maximum number of sweeps (rotation applications)

  Returns map:
  {:eigenvalues [...unsorted...] :vectors V :iterations k}
  where V is an orthogonal matrix whose columns are the (unnormalized but
  numerically unit) eigenvectors corresponding to the returned eigenvalues.

  NOTE:
  * Input matrix is copied; original is left untouched.
  * Off-diagonal search is O(n^2) per iteration – acceptable for small n.
  * Sorting of eigenpairs is intentionally left to callers so they can
    perform domain-specific post-processing (e.g. duplicate collapse in
    complex Hermitian embedding)."
  [A tol max-it]
  (let [[n m] (matrix-shape A)]
    (when (not= n m) (throw (ex-info "jacobi-symmetric requires square matrix" {:shape [n m]})))
    (if (zero? n)
      {:eigenvalues [] :vectors [] :iterations 0}
      (let [A0 (mapv vec A)
            V0 (identity-matrix n)]
        (loop [iter 0 M A0 V V0]
          (if (>= iter max-it)
            {:eigenvalues (mapv #(get-in M [% %]) (range n))
             :vectors (vec (apply mapv vector V))
             :iterations iter}
            (let [[p q val] (reduce (fn [[bp bq bv] [i j]]
                                      (let [aij (Math/abs (double (get-in M [i j])))]
                                        (if (> aij bv) [i j aij] [bp bq bv])))
                                    [0 0 0.0]
                                    (for [i (range n) j (range (inc i) n)] [i j]))]
              (if (< val tol)
                {:eigenvalues (mapv #(get-in M [% %]) (range n))
                 :vectors (vec (apply mapv vector V))
                 :iterations iter}
                (let [app (get-in M [p p]) aqq (get-in M [q q]) apq (get-in M [p q])
                      tau (/ (- aqq app) (* 2.0 apq))
                      t (let [s (if (neg? tau) -1.0 1.0)] (/ s (+ (Math/abs tau) (Math/sqrt (+ 1.0 (* tau tau))))))
                      c (/ 1.0 (Math/sqrt (+ 1.0 (* t t))))
                      s (* t c)
                      rotate-row (fn [M r]
                                   (let [rp (get-in M [r p]) rq (get-in M [r q])]
                                     (-> M
                                         (assoc-in [r p] (- (* c rp) (* s rq)))
                                         (assoc-in [r q] (+ (* s rp) (* c rq))))))
                      M1 (reduce rotate-row M (range n))
                      rotate-col (fn [M r]
                                   (let [pr (get-in M [p r]) qr (get-in M [q r])]
                                     (-> M
                                         (assoc-in [p r] (- (* c pr) (* s qr)))
                                         (assoc-in [q r] (+ (* s pr) (* c qr))))))
                      M2 (reduce rotate-col M1 (range n))
                      apq' (get-in M2 [p q]) app' (get-in M2 [p p]) aqq' (get-in M2 [q q])
                      M3 (-> M2
                             (assoc-in [p p] (- app' (* t apq')))
                             (assoc-in [q q] (+ aqq' (* t apq')))
                             (assoc-in [p q] 0.0)
                             (assoc-in [q p] 0.0))
                      update-V (fn [V r]
                                 (let [vrp (get-in V [r p]) vrq (get-in V [r q])]
                                   (-> V
                                       (assoc-in [r p] (- (* c vrp) (* s vrq)))
                                       (assoc-in [r q] (+ (* s vrp) (* c vrq))))))
                      V1 (reduce update-V V (range n))]
                  (recur (inc iter) M3 V1))))))))))

;; Complex eigenvector phase normalization
;;
;; Eigenvectors of Hermitian matrices are defined up to a global complex phase.
;; For deterministic downstream processing (e.g. comparison in tests, registry
;; lookups) we canonicalize that phase so that the first component with
;; magnitude > tol has zero imaginary part and non-negative real part.

(defn- normalize-complex-phase
  "Normalize global phase of complex vector v (SoA map) so the first
  non-negligible component becomes real and non-negative.

  Parameters:
  - v   {:real [...], :imag [...]} (assumed already L2-normalized or close)
  - tol magnitude threshold to select the reference component.

  Returns new complex vector map with adjusted :real/:imag.

  If all components are (near) zero the vector is returned unchanged."
  [v tol]
  (let [xr (:real v) xi (:imag v)
        n (count xr)
        ;; find reference index
        idx (first (for [i (range n)
                         :let [a (double (nth xr i)) b (double (nth xi i))
                               mag2 (+ (* a a) (* b b))]
                         :when (> mag2 (* tol tol))]
                     i))]
    (if (nil? idx)
      v
      (let [a (double (nth xr idx))
            b (double (nth xi idx))
            ;; Compute phase of reference component a+ib = r e^{i phi}
            phi (Math/atan2 b a)
            c (Math/cos phi)
            s (Math/sin phi)
            ;; Multiply whole vector by e^{-i phi}. For each component ar+i ai:
            ;; (ar + i ai)(cos phi - i sin phi) = (ar c + ai s) + i (ai c - ar s)
            xr' (mapv (fn [ar ai] (+ (* ar c) (* ai s))) xr xi)
            xi' (mapv (fn [ar ai] (- (* ai c) (* ar s))) xr xi)
            ;; Ensure reference component real and non-negative (flip sign if needed)
            ref (nth xr' idx)
            sign (if (neg? ref) -1.0 1.0)
            xr'' (if (= sign 1.0) xr' (mapv #(* sign %) xr'))
            xi'' (if (= sign 1.0) xi' (mapv #(* sign %) xi'))]
        {:real xr'' :imag xi''}))))

;; Nilpotent matrix helpers
;;
;; We often encounter strictly upper (or lower) triangular matrices in
;; quantum gate / circuit constructions (e.g. generators in Lie algebras)
;; which are nilpotent: N^k = 0 for some k ≤ n (matrix size). For a
;; nilpotent matrix N, exp(N) truncates to a finite polynomial:
;;   exp(N) = I + N + N^2/2! + ... + N^{p-1}/(p-1)! where p is the index
;; (smallest p with N^p = 0). Detecting this early avoids the more
;; expensive Padé scaling & squaring and eliminates numerical noise.

(defn- nilpotent-matrix-exp
  "Compute exp(A) for a (real) nilpotent matrix A using a finite series.

  Parameters:
  - A: real square matrix (vector of row vectors)
  - tol: numeric tolerance used to decide when a power becomes the zero matrix
  - max-order (optional): maximum power to test (defaults to matrix dimension)

  Returns:
  Map {:exp E :order k} when A is detected nilpotent with index k (A^k = 0).
  Returns nil if A is not nilpotent up to max-order.

  Implementation notes:
  - We bound the search by n (dimension); if A^n ≠ 0 (within tol) we treat A
    as non-nilpotent for this heuristic.
  - Zero matrix (trivial nilpotent of order 1) handled naturally.
  - Uses Frobenius norm for robust detection (scale invariant wrt sparsity)."
  ([A tol] (nilpotent-matrix-exp A tol (count A)))
  ([A tol max-order]
   (let [[n m] (matrix-shape A)]
     (when (not= n m) (throw (ex-info "nilpotent-matrix-exp requires square matrix" {:shape [n m]})))
     (let [I (identity-matrix n)
           ;; Frobenius norm based zero test
           zero-matrix? (fn [M] (< (real-frobenius-norm M) tol))]
       (loop [k 1 Ak A acc I fact 1.0]
         (cond
           (zero-matrix? Ak)
           {:exp acc :order k}

           (> k max-order)
           nil

           :else
           (let [fact (* fact k)
                 term (real-scale Ak (/ 1.0 fact))
                 acc (real-add acc term)
                 Ak (real-mul Ak A)]
             (recur (inc k) Ak acc fact))))))))

(defn- real-hadamard [A B]
  (mapv (fn [ra rb] (mapv #(* (double %1) (double %2)) ra rb)) A B))

(defn- real-kronecker [A B]
  (let [[ar ac] (matrix-shape A) [br bc] (matrix-shape B)]
    (vec (for [i (range ar)
               bi (range br)]
           (vec (for [j (range ac)
                      bj (range bc)]
                  (* (double (get-in A [i j])) (double (get-in B [bi bj])))))))))

;;
;; Complex (SoA) operations
;;
(defn- complex-add [A B]
  {:real (real-add (:real A) (:real B))
   :imag (real-add (:imag A) (:imag B))})
(defn- complex-sub [A B]
  {:real (real-sub (:real A) (:real B))
   :imag (real-sub (:imag A) (:imag B))})

(defn- complex-scale [A a]
  (if (complex-scalar? a) ; scalar may be complex
    (let [{ar :real ai :imag} a
          Ar (:real A) Ai (:imag A)
          re (mapv (fn [xr xi]
                     (mapv (fn [x y] (- (* ar x) (* ai y))) xr xi)) Ar Ai)
          im (mapv (fn [xr xi]
                     (mapv (fn [x y] (+ (* ar y) (* ai x))) xr xi)) Ar Ai)]
      {:real re :imag im})
    (let [a-real (if (complex-scalar? a) (:real a) (double a))]
      {:real (real-scale (:real A) a-real)
       :imag (real-scale (:imag A) a-real)})))

(defn- complex-mul [A B]
  (let [Ar (:real A) Ai (:imag A) Br (:real B) Bi (:imag B)
        AC (real-mul Ar Br)
        BD (real-mul Ai Bi)
        AD (real-mul Ar Bi)
        BC (real-mul Ai Br)]
    {:real (real-sub AC BD)
     :imag (real-add AD BC)}))

(defn- complex-matvec [A x]
  (let [Ar (:real A) Ai (:imag A) xr (:real x) xi (:imag x)
        mul-r (real-mul Ar (mapv vector xr)) ; treat vector as col matrix
        mul-i (real-mul Ai (mapv vector xi))
        mul-r2 (real-mul Ar (mapv vector xi))
        mul-i2 (real-mul Ai (mapv vector xr))]
    {:real (mapv #(- %1 %2) (mapv first mul-r) (mapv first mul-i))
     :imag (mapv #(+ %1 %2) (mapv first mul-r2) (mapv first mul-i2))}))

(defn- complex-hadamard [A B]
  (let [Ar (:real A) Ai (:imag A) Br (:real B) Bi (:imag B)
        Cr (mapv (fn [ra ia rb ib]
                   (mapv (fn [a i b j] (- (* a b) (* i j))) ra ia rb ib)) Ar Ai Br Bi)
        Ci (mapv (fn [ra ia rb ib]
                   (mapv (fn [a i b j] (+ (* a j) (* i b))) ra ia rb ib)) Ar Ai Br Bi)]
    {:real Cr :imag Ci}))

(defn- complex-kronecker [A B]
  (let [Ar (:real A) Ai (:imag A) Br (:real B) Bi (:imag B)
        RR (real-kronecker Ar Br)
        II (real-kronecker Ai Bi)
        RI (real-kronecker Ar Bi)
        IR (real-kronecker Ai Br)]
    {:real (real-sub RR II)
     :imag (real-add RI IR)}))

(defn- complex-transpose [A]
  {:real (real-transpose (:real A))
   :imag (real-transpose (:imag A))})

(defn- complex-conj-transpose [A]
  {:real (real-transpose (:real A))
   :imag (real-scale (real-transpose (:imag A)) -1.0)})

;; (defn- complex-mul-conjT [A]
;;   (let [Ah (complex-conj-transpose A)]
;;     (complex-mul Ah A))) ; unused helper (left as reference)

;; Complex Gaussian elimination helpers
;;
;; These implement partial pivot Gaussian elimination for a single RHS vector
;; and Gauss-Jordan inversion for complex matrices represented in SoA form.
;; They are intentionally straightforward (no blocking / BLAS) and target
;; small to medium matrix sizes.

(defn- complex-forward-elim
  "Forward elimination (partial pivot) for complex A x = b.
  Ar/Ai: matrix parts, br/bi: RHS parts. Returns [Ar Ai br bi] in row echelon form."
  [Ar Ai br bi]
  (let [n (count Ar)]
    (loop [k 0 Ar Ar Ai Ai br br bi bi]
      (if (= k n)
        [Ar Ai br bi]
        (let [pivot (apply max-key #(Math/hypot (get-in Ar [% k]) (get-in Ai [% k])) (range k n))
              swap-row (fn [M] (if (not= pivot k) (-> M (assoc k (M pivot)) (assoc pivot (M k))) M))
              Ar (swap-row Ar) Ai (swap-row Ai)
              br (if (not= pivot k) (-> br (assoc k (br pivot)) (assoc pivot (br k))) br)
              bi (if (not= pivot k) (-> bi (assoc k (bi pivot)) (assoc pivot (bi k))) bi)
              akk-r (get-in Ar [k k]) akk-i (get-in Ai [k k])
              denom (+ (* akk-r akk-r) (* akk-i akk-i))]
          (when (zero? denom) (throw (ex-info "Singular complex matrix (zero pivot)" {:k k})))
          (let [rowr (Ar k) rowi (Ai k)
                Ar (assoc Ar k (mapv (fn [ar ai] (/ (+ (* ar akk-r) (* ai akk-i)) denom)) rowr rowi))
                Ai (assoc Ai k (mapv (fn [ar ai] (/ (- (* ai akk-r) (* ar akk-i)) denom)) rowr rowi))
                brk (br k) bik (bi k)
                nr (/ (+ (* brk akk-r) (* bik akk-i)) denom)
                ni (/ (- (* bik akk-r) (* brk akk-i)) denom)
                br (assoc br k nr) bi (assoc bi k ni)
                [Ar Ai br bi] (loop [i (inc k) Ar Ar Ai Ai br br bi bi]
                                (if (= i n)
                                  [Ar Ai br bi]
                                  (let [f-r (get-in Ar [i k]) f-i (get-in Ai [i k])]
                                    (if (and (zero? f-r) (zero? f-i))
                                      (recur (inc i) Ar Ai br bi)
                                      (let [rowk-r (Ar k) rowk-i (Ai k)
                                            Ar-rowi (vec (map-indexed (fn [j arik]
                                                                        (- arik (- (* f-r (rowk-r j)) (* f-i (rowk-i j))))) (Ar i)))
                                            Ai-rowi (vec (map-indexed (fn [j aiik]
                                                                        (- aiik (+ (* f-r (rowk-i j)) (* f-i (rowk-r j))))) (Ai i)))
                                            br (assoc br i (- (br i) (- (* f-r (br k)) (* f-i (bi k)))))
                                            bi (assoc bi i (- (bi i) (+ (* f-r (bi k)) (* f-i (br k)))))
                                            Ar (assoc Ar i Ar-rowi) Ai (assoc Ai i Ai-rowi)]
                                        (recur (inc i) Ar Ai br bi))))))]
            (recur (inc k) Ar Ai br bi)))))))

(defn- complex-back-sub
  "Back substitution on upper-triangular complex system."
  [Ar Ai br bi]
  (let [n (count Ar) xr (double-array n) xi (double-array n)]
    (loop [i (dec n)]
      (when (>= i 0)
        (let [sumr (reduce + (for [j (range (inc i) n)] (- (* (get-in Ar [i j]) (aget xr j)) (* (get-in Ai [i j]) (aget xi j)))))
              sumi (reduce + (for [j (range (inc i) n)] (- (* (get-in Ar [i j]) (aget xi j)) (* (get-in Ai [i j]) (aget xr j)))))
              arii (get-in Ar [i i]) aiii (get-in Ai [i i])
              den (+ (* arii arii) (* aiii aiii))
              nr (- (br i) sumr) ni (- (bi i) sumi)
              xr-i (/ (+ (* nr arii) (* ni aiii)) den)
              xi-i (/ (- (* ni arii) (* nr aiii)) den)]
          (aset xr i xr-i) (aset xi i xi-i) (recur (dec i)))))
    {:real (vec xr) :imag (vec xi)}))

(defn- complex-solve-vector
  "Solve complex linear system A x = b and return complex vector representation."
  [A b]
  (let [Ar (mapv vec (:real A)) Ai (mapv vec (:imag A))
        b* (if (complex-vector? b) b (ensure-complex-vector b))
        br (vec (:real b*)) bi (vec (:imag b*))
        [Ar Ai br bi] (complex-forward-elim Ar Ai br bi)]
    (complex-back-sub Ar Ai br bi)))

(defn- complex-inverse
  "Inverse of complex matrix via Gauss-Jordan. Returns SoA map.
  FIX: refactored elimination loop to avoid endless inner loop pattern."
  [A]
  (let [Ar (mapv vec (:real A)) Ai (mapv vec (:imag A))
        n (count Ar) Ir (identity-matrix n) Ii (vec (repeat n (vec (repeat n 0.0))))]
    (loop [k 0 Ar Ar Ai Ai Ir Ir Ii Ii]
      (if (= k n)
        {:real Ir :imag Ii}
        (let [pivot (apply max-key #(Math/hypot (get-in Ar [% k]) (get-in Ai [% k])) (range k n))
              swap-row (fn [M] (if (not= pivot k) (-> M (assoc k (M pivot)) (assoc pivot (M k))) M))
              Ar (swap-row Ar) Ai (swap-row Ai) Ir (swap-row Ir) Ii (swap-row Ii)
              akk-r (get-in Ar [k k]) akk-i (get-in Ai [k k])
              denom (+ (* akk-r akk-r) (* akk-i akk-i))]
          (when (zero? denom) (throw (ex-info "Singular complex matrix (inverse)" {:k k})))
          (let [rowr (Ar k) rowi (Ai k) ir-row (Ir k) ii-row (Ii k)
                Ar (assoc Ar k (mapv (fn [ar ai] (/ (+ (* ar akk-r) (* ai akk-i)) denom)) rowr rowi))
                Ai (assoc Ai k (mapv (fn [ar ai] (/ (- (* ai akk-r) (* ar akk-i)) denom)) rowr rowi))
                Ir (assoc Ir k (mapv (fn [ar ai] (/ (+ (* ar akk-r) (* ai akk-i)) denom)) ir-row ii-row))
                Ii (assoc Ii k (mapv (fn [ar ai] (/ (- (* ai akk-r) (* ar akk-i)) denom)) ir-row ii-row))
                [Ar Ai Ir Ii] (loop [i 0 Ar Ar Ai Ai Ir Ir Ii Ii]
                                (if (= i n)
                                  [Ar Ai Ir Ii]
                                  (if (= i k)
                                    (recur (inc i) Ar Ai Ir Ii)
                                    (let [f-r (get-in Ar [i k]) f-i (get-in Ai [i k])]
                                      (if (and (zero? f-r) (zero? f-i))
                                        (recur (inc i) Ar Ai Ir Ii)
                                        (let [rowk-r (Ar k) rowk-i (Ai k)
                                              irk-row (Ir k) iik-row (Ii k)
                                              Ar-rowi (vec (map-indexed (fn [j arik]
                                                                          (let [rkj (nth rowk-r j) ikj (nth rowk-i j)]
                                                                            (- arik (- (* f-r rkj) (* f-i ikj))))) (Ar i)))
                                              Ai-rowi (vec (map-indexed (fn [j aiik]
                                                                          (let [rkj (nth rowk-r j) ikj (nth rowk-i j)]
                                                                            (- aiik (+ (* f-r ikj) (* f-i rkj))))) (Ai i)))
                                              Ir-rowi (vec (map-indexed (fn [j irik]
                                                                          (let [irkj (nth irk-row j) iikj (nth iik-row j)]
                                                                            (- irik (- (* f-r irkj) (* f-i iikj))))) (Ir i)))
                                              Ii-rowi (vec (map-indexed (fn [j iik]
                                                                          (let [irkj (nth irk-row j) iikj (nth iik-row j)]
                                                                            (- iik (+ (* f-r iikj) (* f-i irkj))))) (Ii i)))
                                              Ar (assoc Ar i Ar-rowi) Ai (assoc Ai i Ai-rowi)
                                              Ir (assoc Ir i Ir-rowi) Ii (assoc Ii i Ii-rowi)]
                                          (recur (inc i) Ar Ai Ir Ii)))))))]
            (recur (inc k) Ar Ai Ir Ii)))))))

;;
;; Inner product & norms
;;
(defn- real-inner [x y]
  (reduce + (map #(* (double %1) (double %2)) x y)))

(defn- complex-inner [x y]
  (let [xr (:real x) xi (:imag x) yr (:real y) yi (:imag y)
        re (reduce + (map (fn [a b c d] (+ (* a b) (* c d))) xr yr xi yi))
        im (reduce + (map (fn [a b c d] (- (* a d) (* c b))) xr yr xi yi))]
    (make-complex re im)))

;;
;; Hermitian / unitary / PSD
;;
(defn- close-matrices? [A B tol]
  (let [[r c] (matrix-shape A)]
    (every? true?
            (for [i (range r) j (range c)]
              (< (Math/abs (double (- (if (complex-matrix? A)
                                        (get-in (:real A) [i j])
                                        (get-in A [i j]))
                                      (if (complex-matrix? B)
                                        (get-in (:real B) [i j])
                                        (get-in B [i j]))))) tol)))))

(defn- hermitian-real? [A tol]
  (let [[r c] (matrix-shape A)]
    (and (= r c)
         (every? true?
                 (for [i (range r) j (range i r)]
                   (< (Math/abs (double (- (get-in A [i j]) (get-in A [j i])))) tol))))))

(defn- hermitian-complex? [A tol]
  (let [Ar (:real A) Ai (:imag A) n (count Ar)]
    (and (= n (count (first Ar))) ; square
         (every? true?
                 (for [i (range n) j (range i n)]
                   (let [aij-r (get-in Ar [i j]) aij-i (get-in Ai [i j])
                         aji-r (get-in Ar [j i]) aji-i (get-in Ai [j i])]
                     (and (< (Math/abs (double (- aij-r aji-r))) tol)
                          (< (Math/abs (double (+ aij-i aji-i))) tol))))))))

(defn- power-iteration-largest-eigenvalue-real [A iterations tol]
  (let [shape (matrix-shape A)
        n (first shape)
        v0 (vec (repeat n (/ 1.0 (Math/sqrt n))))]
    (loop [v v0 i 0]
      (if (>= i iterations)
        (let [Av (mapv (fn [row] (reduce + (map * row v))) A)
              lambda (/ (real-inner Av v) (real-inner v v))]
          lambda)
        (let [Av (mapv (fn [row] (reduce + (map * row v))) A)
              nrm (Math/sqrt (real-inner Av Av))
              v-next (if (pos? nrm) (mapv #(/ % nrm) Av) v)]
          (if (and (> i 2) (< (Math/abs (- (real-inner v v-next) 1.0)) tol))
            (recur v-next iterations)
            (recur v-next (inc i))))))))

;;
;; Positive semidefinite: check Hermitian + all principal minors >=0 (small n)
;;
(defn- psd-real? [A tol]
  (let [[_n _m] (matrix-shape A)] ; shape currently unused; retained for future optimizations
    (when (hermitian-real? A tol)
      ;; Eigenvalue based PSD check (Jacobi) – ensures correctness for any size.
      (let [{:keys [eigenvalues]}
            (let [jacobi-wrapper
                  (fn jacobi-wrapper [M]
                    (let [[n _] (matrix-shape M)
                          tol* (* 10 tol)
                          max-it 200
                          ;; Copy matrix to mutable (vector of vectors)
                          A (mapv vec M)
                          V (identity-matrix n)]
                      (if (zero? n)
                        {:eigenvalues [] :eigenvectors []}
                        (loop [iter 0 A A V V]
                          (if (>= iter max-it)
                            {:eigenvalues (mapv #(get-in A [% %]) (range n))
                             :eigenvectors (vec (apply mapv vector V))}
                            (let [;; find largest off-diagonal
                                  [p q val] (reduce (fn [[bp bq bv] [i j]]
                                                      (let [aij (Math/abs (double (get-in A [i j])))]
                                                        (if (> aij bv) [i j aij] [bp bq bv])))
                                                    [0 0 0.0]
                                                    (for [i (range n) j (range (inc i) n)] [i j]))]
                              (if (< val tol*)
                                {:eigenvalues (mapv #(get-in A [% %]) (range n))
                                 :eigenvectors (vec (apply mapv vector V))}
                                (let [app (get-in A [p p]) aqq (get-in A [q q]) apq (get-in A [p q])
                                      tau (/ (- aqq app) (* 2.0 apq))
                                      t (let [s (if (neg? tau) -1.0 1.0)] (/ s (+ (Math/abs tau) (Math/sqrt (+ 1.0 (* tau tau))))))
                                      c (/ 1.0 (Math/sqrt (+ 1.0 (* t t))))
                                      s (* t c)
                                      ;; rotate A in-place (functional updates)
                                      rotate-row (fn [A r]
                                                   (let [arp (get-in A [r p]) arq (get-in A [r q])]
                                                     (-> A
                                                         (assoc-in [r p] (- (* c arp) (* s arq)))
                                                         (assoc-in [r q] (+ (* s arp) (* c arq))))))
                                      A (reduce rotate-row A (range n))
                                      rotate-col (fn [A r]
                                                   (let [arp (get-in A [p r]) arq (get-in A [q r])] ; original before overwrite? Accept symmetric usage.
                                                     (-> A
                                                         (assoc-in [p r] (- (* c arp) (* s arq)))
                                                         (assoc-in [q r] (+ (* s arp) (* c arq))))))
                                      A (reduce rotate-col A (range n))
                                      app' (get-in A [p p]) aqq' (get-in A [q q])
                                      apq' (get-in A [p q])
                                      A (-> A (assoc-in [p p] (- app' (* t apq'))) ; Jacobi formulas
                                            (assoc-in [q q] (+ aqq' (* t apq'))) (assoc-in [p q] 0.0) (assoc-in [q p] 0.0))
                                      ;; update V (eigenvectors)
                                      update-V (fn [V r]
                                                 (let [vrp (get-in V [r p]) vrq (get-in V [r q])]
                                                   (-> V (assoc-in [r p] (- (* c vrp) (* s vrq)))
                                                       (assoc-in [r q] (+ (* s vrp) (* c vrq))))))
                                      V (reduce update-V V (range n))]
                                  (recur (inc iter) A V)))))))) A)]
              (jacobi-wrapper A))
            min-eig (reduce min Double/POSITIVE_INFINITY eigenvalues)]
        (>= min-eig (* -1.0 1e-9))))))

;; Spectral norm helpers (public for reuse in higher-level analyses)
(defn spectral-norm-real
  "Compute spectral norm (largest singular value) of real matrix A via power iteration
  on A^T A tracking Rayleigh quotient convergence.

  Parameters:
  - A: real m x n matrix

  Returns largest singular value (double)." [A]
  (let [AtA (real-mul (real-transpose A) A)
        [n _] (matrix-shape AtA)
        v0 (vec (repeat n (/ 1.0 (Math/sqrt (max 1 n)))))
        tol (* 1e-12 (inc n))
        max-it 200
        zero-matrix? (every? (fn [row] (every? zero? row)) AtA)]
    (if zero-matrix?
      0.0
      (loop [k 0 v v0 lambda-prev nil]
        (let [Av (mapv (fn [row] (reduce + (map * row v))) AtA)
              nrm (Math/sqrt (reduce + (map #(* (double %) (double %)) Av)))
              v' (if (pos? nrm) (mapv #(/ % nrm) Av) v)
              Av' (mapv (fn [row] (reduce + (map * row v'))) AtA)
              lambda (real-inner v' Av')
              conv? (and lambda-prev (< (Math/abs (- lambda lambda-prev)) (* tol (max 1.0 (Math/abs lambda)))))]
          (if (or (>= k max-it) conv?)
            (Math/sqrt (max 0.0 (double lambda)))
            (recur (inc k) v' lambda)))))))

(defn spectral-norm-complex
  "Compute spectral norm of complex matrix A via power iteration on A^H A with Rayleigh quotient convergence." [A]
  (let [n (count (:real A))
        x0 {:real (vec (repeat n (/ 1.0 (Math/sqrt n)))) :imag (vec (repeat n 0.0))}
        tol 1e-12
        max-it 200
        Ah (complex-conj-transpose A)]
    (loop [k 0 x x0 lambda-prev nil]
      (let [Ax (complex-matvec A x)
            AhAx (complex-matvec Ah Ax)
            lambda (let [num-r (reduce + (map (fn [a b c d] (+ (* a b) (* c d))) (:real x) (:real AhAx) (:imag x) (:imag AhAx)))]
                     (double num-r))
            nr (Math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) (:real AhAx) (:imag AhAx))))
            x' {:real (mapv #(/ % nr) (:real AhAx)) :imag (mapv #(/ % nr) (:imag AhAx))}
            conv? (and lambda-prev (< (Math/abs (- lambda lambda-prev)) (* tol (max 1.0 (Math/abs lambda)))))]
        (if (or (>= k max-it) conv?)
          (Math/sqrt (max 0.0 lambda))
          (recur (inc k) x' lambda))))))

;;;
;;; Backend record & protocol implementations
;;;
(defrecord ClojureMathBackend [tolerance config])

;;
;; Complex scalar protocol
;;
(extend-protocol proto/Complex
  Number
  (real [x] (double x))

  (imag [_] 0.0)

  (conjugate [x] x)

  (complex? [_] false)

  clojure.lang.IPersistentMap
  (real [m] (if (contains? m :real) (double (:real m)) (throw (ex-info "Not a complex map" {:value m}))))

  (imag [m] (if (contains? m :imag) (double (:imag m)) 0.0))

  (conjugate [m] (if (and (contains? m :real) (contains? m :imag)) (update m :imag #(- (double %))) m))

  (complex? [m] (and (contains? m :real) (contains? m :imag))))

;;
;; BackendAdapter - conversion between FastMath Vec2 and SoA representations
;;
(extend-protocol proto/BackendAdapter
  ClojureMathBackend
  
  (vector->backend [_ v]
    "Convert QClojure vector (FastMath Vec2 complex numbers) to clojure-math SoA representation.
    
    Converts a vector of FastMath Vec2 complex numbers to the SoA (Split-of-Arrays) format
    used internally by the clojure-math backend. Each Vec2 object is decomposed into
    its real and imaginary components.
    
    Parameters:
    - v: Vector of FastMath Vec2 complex numbers or SoA map
    
    Returns:
    SoA map {:real [...] :imag [...]} or passes through non-Vec2 vectors unchanged
    
    Throws:
    ExceptionInfo for malformed or incompatible input data"
    (cond
      ;; Already in SoA format - validate structure
      (complex-vector? v) v
      
      ;; Regular Clojure vector of numbers  
      (and (vector? v) (every? number? v))
      {:real (mapv double v) :imag (mapv (constantly 0.0) v)}
      
      ;; Vector of FastMath Vec2 complex numbers
      (and (vector? v) (every? #(instance? fastmath.vector.Vec2 %) v))
      (try
        {:real (mapv #(double (fc/re %)) v)
         :imag (mapv #(double (fc/im %)) v)}
        (catch Exception e
          (throw (ex-info "Failed to convert Vec2 vector to SoA format" 
                          {:input-vector v :error (.getMessage e)}))))
      
      ;; Empty vector
      (and (vector? v) (empty? v))
      {:real [] :imag []}
      
      ;; Nil input
      (nil? v)
      (throw (ex-info "Cannot convert nil to backend vector format" {:input v}))
      
      ;; Pass through other formats unchanged
      :else v))

  (backend->vector [_ v]
    "Convert clojure-math SoA representation back to QClojure vector format.
    
    Converts the internal SoA representation back to a vector of FastMath Vec2
    complex numbers for use by quantum state and gate operations.
    
    Parameters:
    - v: SoA map {:real [...] :imag [...]} or other vector format
    
    Returns:
    Vector of FastMath Vec2 complex numbers or passes through unchanged
    
    Throws:
    ExceptionInfo for malformed SoA data or conversion failures"
    (cond
      ;; SoA format - convert to Vec2 complex numbers
      (complex-vector? v)
      (let [real-part (:real v)
            imag-part (:imag v)]
        (when (not= (count real-part) (count imag-part))
          (throw (ex-info "SoA vector real and imaginary parts have different lengths"
                          {:real-length (count real-part) 
                           :imag-length (count imag-part)
                           :input v})))
        (try
          (mapv #(fc/complex %1 %2) real-part imag-part)
          (catch Exception e
            (throw (ex-info "Failed to convert SoA to Vec2 vector" 
                            {:input-vector v :error (.getMessage e)})))))
      
      ;; Real vector - convert to Vec2 with zero imaginary parts
      (and (vector? v) (every? number? v))
      (try
        (mapv #(fc/complex (double %) 0.0) v)
        (catch Exception e
          (throw (ex-info "Failed to convert real vector to Vec2 format"
                          {:input-vector v :error (.getMessage e)}))))
      
      ;; Empty vector
      (and (vector? v) (empty? v))
      []
      
      ;; Nil input
      (nil? v)
      (throw (ex-info "Cannot convert nil from backend vector format" {:input v}))
      
      ;; Pass through other formats unchanged
      :else v))

  (matrix->backend [_ m]
    "Convert QClojure matrix (Vec2 or vector format) to clojure-math SoA representation.
    
    Converts matrices from various input formats to the SoA representation used
    internally by the clojure-math backend. Handles both Vec2-based and plain
    vector-based matrix representations.
    
    Parameters:
    - m: Matrix in various formats (Vec2, vector, or SoA)
    
    Returns:
    SoA map {:real [[...]] :imag [[...]]} or passes through unchanged
    
    Throws:
    ExceptionInfo for malformed matrices or conversion failures"
    (cond
      ;; Already in SoA format - validate structure
      (complex-matrix? m) 
      (let [real-matrix (:real m)
            imag-matrix (:imag m)]
        (when (not= (count real-matrix) (count imag-matrix))
          (throw (ex-info "SoA matrix real and imaginary parts have different row counts"
                          {:real-rows (count real-matrix)
                           :imag-rows (count imag-matrix)
                           :input m})))
        (when (and (not-empty real-matrix) (not-empty imag-matrix))
          (let [real-cols (map count real-matrix)
                imag-cols (map count imag-matrix)]
            (when (not= real-cols imag-cols)
              (throw (ex-info "SoA matrix real and imaginary parts have different column structure"
                              {:real-cols real-cols
                               :imag-cols imag-cols
                               :input m})))))
        m)
      
      ;; Matrix of FastMath Vec2 complex numbers
      (and (vector? m) (every? vector? m) 
           (every? #(every? (fn [x] (instance? fastmath.vector.Vec2 x)) %) m))
      (try
        ;; Validate rectangular matrix structure
        (when (not-empty m)
          (let [row-lengths (map count m)]
            (when (not (apply = row-lengths))
              (throw (ex-info "Matrix rows have different lengths"
                              {:row-lengths row-lengths :input m})))))
        {:real (mapv (fn [row] (mapv #(double (fc/re %)) row)) m)
         :imag (mapv (fn [row] (mapv #(double (fc/im %)) row)) m)}
        (catch Exception e
          (throw (ex-info "Failed to convert Vec2 matrix to SoA format"
                          {:input-matrix m :error (.getMessage e)}))))
      
      ;; Matrix of real numbers
      (and (vector? m) (every? vector? m) (every? #(every? number? %) m))
      (try
        ;; Validate rectangular matrix structure
        (when (not-empty m)
          (let [row-lengths (map count m)]
            (when (not (apply = row-lengths))
              (throw (ex-info "Matrix rows have different lengths"
                              {:row-lengths row-lengths :input m})))))
        {:real (mapv (fn [row] (mapv double row)) m)
         :imag (mapv (fn [row] (mapv (constantly 0.0) row)) m)}
        (catch Exception e
          (throw (ex-info "Failed to convert real matrix to SoA format"
                          {:input-matrix m :error (.getMessage e)}))))
      
      ;; Empty matrix
      (and (vector? m) (empty? m))
      {:real [] :imag []}
      
      ;; Nil input
      (nil? m)
      (throw (ex-info "Cannot convert nil to backend matrix format" {:input m}))
      
      ;; Pass through other formats unchanged
      :else m))

  (backend->matrix [_ m]
    "Convert clojure-math SoA representation back to QClojure matrix format.
    
    Converts the internal SoA representation back to a matrix of FastMath Vec2
    complex numbers for use by quantum gate operations.
    
    Parameters:
    - m: SoA map {:real [[...]] :imag [[...]]} or other matrix format
    
    Returns:
    Matrix of FastMath Vec2 complex numbers or passes through unchanged
    
    Throws:
    ExceptionInfo for malformed SoA data or conversion failures"
    (cond
      ;; SoA format - convert to Vec2 complex numbers
      (complex-matrix? m)
      (let [real-matrix (:real m)
            imag-matrix (:imag m)]
        (when (not= (count real-matrix) (count imag-matrix))
          (throw (ex-info "SoA matrix real and imaginary parts have different row counts"
                          {:real-rows (count real-matrix)
                           :imag-rows (count imag-matrix)
                           :input m})))
        (when (and (not-empty real-matrix) (not-empty imag-matrix))
          (let [real-cols (map count real-matrix)
                imag-cols (map count imag-matrix)]
            (when (not= real-cols imag-cols)
              (throw (ex-info "SoA matrix real and imaginary parts have different column structure"
                              {:real-cols real-cols
                               :imag-cols imag-cols
                               :input m})))))
        (try
          (mapv (fn [real-row imag-row]
                  (mapv #(fc/complex %1 %2) real-row imag-row))
                real-matrix imag-matrix)
          (catch Exception e
            (throw (ex-info "Failed to convert SoA to Vec2 matrix"
                            {:input-matrix m :error (.getMessage e)})))))
      
      ;; Real matrix - convert to Vec2 with zero imaginary parts
      (and (vector? m) (every? vector? m) (every? #(every? number? %) m))
      (try
        ;; Validate rectangular matrix structure
        (when (not-empty m)
          (let [row-lengths (map count m)]
            (when (not (apply = row-lengths))
              (throw (ex-info "Matrix rows have different lengths"
                              {:row-lengths row-lengths :input m})))))
        (mapv (fn [row] (mapv #(fc/complex (double %) 0.0) row)) m)
        (catch Exception e
          (throw (ex-info "Failed to convert real matrix to Vec2 format"
                          {:input-matrix m :error (.getMessage e)}))))
      
      ;; Empty matrix
      (and (vector? m) (empty? m))
      []
      
      ;; Nil input
      (nil? m)
      (throw (ex-info "Cannot convert nil from backend matrix format" {:input m}))
      
      ;; Pass through other formats unchanged
      :else m))

  (scalar->backend [_ s]
    "Convert FastMath Vec2 complex scalar to clojure-math SoA representation.
    
    Converts Vec2 complex numbers to the internal SoA representation
    {:real r :imag i} for use by clojure-math operations.
    
    Parameters:
    - s: FastMath Vec2 complex number, real number, or SoA scalar
    
    Returns:
    SoA scalar map {:real r :imag i} or passes through unchanged
    
    Throws:
    ExceptionInfo for invalid scalar input"
    (cond
      ;; Vec2 complex scalar
      (instance? fastmath.vector.Vec2 s)
      {:real (fc/re s) :imag (fc/im s)}
      
      ;; Real scalar
      (number? s)
      {:real (double s) :imag 0.0}
      
      ;; Already in SoA format
      (complex-scalar? s)
      s
      
      ;; Nil input
      (nil? s)
      (throw (ex-info "Cannot convert nil to backend scalar format" {:input s}))
      
      ;; Invalid input
      :else
      (throw (ex-info "Cannot convert to backend scalar format" 
                      {:input s :type (type s)}))))

  (backend->scalar [_ s]
    "Convert clojure-math SoA scalar representation back to FastMath Vec2.
    
    Converts the internal SoA representation back to FastMath Vec2
    complex numbers for use by quantum operations.
    
    Parameters:
    - s: SoA scalar map {:real r :imag i}, number, or Vec2 complex
    
    Returns:
    FastMath Vec2 complex number or passes through unchanged
    
    Throws:
    ExceptionInfo for invalid scalar input"
    (cond
      ;; SoA scalar format
      (complex-scalar? s)
      (try
        (fc/complex (:real s) (:imag s))
        (catch Exception e
          (throw (ex-info "Failed to convert SoA scalar to Vec2 format"
                          {:input-scalar s :error (.getMessage e)}))))
      
      ;; Real scalar
      (number? s)
      (fc/complex (double s) 0.0)
      
      ;; Already Vec2 format
      (instance? fastmath.vector.Vec2 s)
      s
      
      ;; Nil input
      (nil? s)
      (throw (ex-info "Cannot convert nil from backend scalar format" {:input s}))
      
      ;; Invalid input
      :else
      (throw (ex-info "Cannot convert from backend scalar format" 
                      {:input s :type (type s)})))))

;;
;; MatrixAlgebra
;;
(extend-protocol proto/MatrixAlgebra
  ClojureMathBackend
  (shape [_ A] (matrix-shape A))

  (add [_ A B]
    (cond (and (complex-matrix? A) (complex-matrix? B)) (complex-add A B)
          (complex-matrix? A) (complex-add A (ensure-complex-matrix B))
          (complex-matrix? B) (complex-add (ensure-complex-matrix A) B)
          :else (real-add A B)))

  (subtract [_ A B]
    (cond (and (complex-matrix? A) (complex-matrix? B)) (complex-sub A B)
          (complex-matrix? A) (complex-sub A (ensure-complex-matrix B))
          (complex-matrix? B) (complex-sub (ensure-complex-matrix A) B)
          :else (real-sub A B)))

  (scale [_ A alpha]
    (cond (complex-matrix? A) (complex-scale A alpha)
          (complex-scalar? alpha) (complex-scale (ensure-complex-matrix A) alpha)
          :else (real-scale A alpha)))

  (negate [b A] (proto/scale b A -1.0))

  (matrix-multiply [_ A B]
    (cond (and (complex-matrix? A) (complex-matrix? B)) (complex-mul A B)
          (complex-matrix? A) (complex-mul A (ensure-complex-matrix B))
          (complex-matrix? B) (complex-mul (ensure-complex-matrix A) B)
          :else (real-mul A B)))

  (matrix-vector-product [_ A x]
    ;; Correct complex matrix-vector multiplication uses cross terms
    (cond (and (complex-matrix? A) (complex-vector? x)) (complex-matvec A x)
          (complex-matrix? A) (complex-matvec A (ensure-complex-vector x))
          (complex-vector? x) (complex-matvec (ensure-complex-matrix A) x)
          :else (mapv (fn [row] (reduce + (map * row x))) A)))

  (outer-product [b x y]
    (cond (and (complex-vector? x) (complex-vector? y))
          (let [xr (:real x) xi (:imag x)
                yr (:real y) yi (:imag y)
                ;; (x y^H)_{ij} = (xr_i + i xi_i)(yr_j - i yi_j)
                real (vec (for [i (range (count xr))]
                            (vec (for [j (range (count yr))]
                                   (+ (* (nth xr i) (nth yr j)) (* (nth xi i) (nth yi j)))))))
                imag (vec (for [i (range (count xr))]
                            (vec (for [j (range (count yr))]
                                   (- (* (nth xi i) (nth yr j)) (* (nth xr i) (nth yi j)))))))]
            {:real real :imag imag})
          (complex-vector? x) (proto/outer-product b x (ensure-complex-vector y))
          (complex-vector? y) (proto/outer-product b (ensure-complex-vector x) y)
          :else (vec (for [a x] (vec (for [b y] (* (double a) (double b))))))))

  (hadamard [_ A B]
    (cond (and (complex-matrix? A) (complex-matrix? B)) (complex-hadamard A B)
          (complex-matrix? A) (complex-hadamard A (ensure-complex-matrix B))
          (complex-matrix? B) (complex-hadamard (ensure-complex-matrix A) B)
          :else (real-hadamard A B)))

  (kronecker [_ A B]
    (cond (and (complex-matrix? A) (complex-matrix? B)) (complex-kronecker A B)
          (complex-matrix? A) (complex-kronecker A (ensure-complex-matrix B))
          (complex-matrix? B) (complex-kronecker (ensure-complex-matrix A) B)
          :else (real-kronecker A B)))

  (transpose [_ A]
    (if (complex-matrix? A) (complex-transpose A) (real-transpose A)))

  (conjugate-transpose [_ A]
    (if (complex-matrix? A) (complex-conj-transpose A) (real-transpose A)))

  (trace [_ A]
    (if (complex-matrix? A)
      (let [n (count (:real A))]
        (make-complex (reduce + (map (fn [i] (get-in (:real A) [i i])) (range n)))
                      (reduce + (map (fn [i] (get-in (:imag A) [i i])) (range n)))))
      (let [n (count A)] (reduce + (map (fn [i] (get-in A [i i])) (range n))))))

  (inner-product [_ x y]
    (cond (and (complex-vector? x) (complex-vector? y)) (complex-inner x y)
          (complex-vector? x) (complex-inner x (ensure-complex-vector y))
          (complex-vector? y) (complex-inner (ensure-complex-vector x) y)
          :else (real-inner x y)))

  (norm2 [b x]
    (let [ip (proto/inner-product b x x)]
      (if (complex-scalar? ip)
        ;; <x|x> for a valid inner product should be real non-negative; imaginary part ≈ 0.
        ;; Old code incorrectly computed sqrt(re^2 + im^2) which overestimates.
        (let [re (double (:real ip))
              re (if (neg? re) (Math/abs re) re)]
          (Math/sqrt re))
        (Math/sqrt (double ip)))))

  (solve-linear-system [backend A b]
    ;; If either A or b is complex, promote to complex solve path.
    (if (or (complex-matrix? A) (complex-vector? b))
      (let [A* (if (complex-matrix? A) A (ensure-complex-matrix A))
            b* (if (complex-vector? b) b (ensure-complex-vector b))
            [n n2] (matrix-shape A*)]
        (when (not= n n2) (throw (ex-info "A must be square" {:shape [n n2]})))
        (complex-solve-vector A* b*))
      (let [[n n2] (matrix-shape A)]
        (when (not= n n2) (throw (ex-info "A must be square" {:shape [n n2]})))
        (let [{:keys [P L U]} (proto/lu-decomposition backend A)
              b* (vec (map #(nth b %) P))
              y (real-forward-sub L b*)
              x (real-back-sub U y)]
          x))))

  (inverse [backend A]
    (if (complex-matrix? A)
      (complex-inverse A)
      (let [[n n2] (matrix-shape A)]
        (when (not= n n2) (throw (ex-info "A must be square" {:shape [n n2]})))
        (let [{:keys [P L U]} (proto/lu-decomposition backend A)
              inv-cols (for [col (range n)]
                         (let [e (vec (for [i (range n)] (if (= i col) 1.0 0.0)))
                               b (vec (map #(nth e %) P))
                               y (real-forward-sub L b)
                               x (real-back-sub U y)]
                           x))
              X (vec (apply mapv vector inv-cols))]
          X))))

  (hermitian?
    ([b A] (proto/hermitian? b A (tolerance* b)))
    ([_ A eps]
     (let [tol (double (or eps default-tolerance))]
       (if (complex-matrix? A)
         (hermitian-complex? A tol)
         (hermitian-real? A tol)))))

  (unitary?
    ([b U] (proto/unitary? b U (tolerance* b)))
    ([_ U eps]
     (let [tol (double (or eps default-tolerance))
           [n m] (matrix-shape U)]
       (if (not= n m)
         false
         (let [Uh (if (complex-matrix? U) (complex-conj-transpose U) (real-transpose U))
               P (if (complex-matrix? U) (complex-mul Uh U) (real-mul Uh U))
               I (identity-matrix n)]
           (if (complex-matrix? P)
             (close-matrices? (:real P) I tol)
             (close-matrices? P I tol)))))))

  (positive-semidefinite? [b A]
    (let [tol (tolerance* b)]
      (if (complex-matrix? A)
        (if (hermitian-complex? A tol)
          (let [{:keys [eigenvalues]} (proto/eigen-hermitian b A)]
            (every? #(>= % (- tol)) eigenvalues))
          false)
        (if (hermitian-real? A tol)
          (let [{:keys [eigenvalues]} (proto/eigen-hermitian b A)]
            (every? #(>= % (- tol)) eigenvalues))
          false)))))

;;
;; MatrixDecompositions (basic / partial)
;;
(extend-protocol proto/MatrixDecompositions
  ClojureMathBackend
  (eigen-hermitian [_ A]
    "Eigen-decomposition for Hermitian matrices (real symmetric & complex Hermitian).

    Returns map {:eigenvalues [λ₀≤…≤λₙ₋₁] :eigenvectors [v₀ …]} with eigenvalues
    sorted ascending. Eigenvectors:
    - Real case: plain real vectors.
    - Complex case: SoA maps {:real [...], :imag [...]} with deterministic
      global phase (first non-negligible component real and ≥ 0).

    Implementation strategy:
    1. Real case delegates to shared jacobi-symmetric helper then sorts pairs.
    2. Complex case embeds H = X + iY (Xᵀ=X, Yᵀ=-Y) into real symmetric block
       M = [[X -Y][Y X]] (dimension 2n). jacobi-symmetric gives eigenpairs
       (λ, w). Each original eigenvalue appears twice (paired due to embedding);
       we collapse duplicates within tolerance and reconstruct complex vectors
       v = x + i y where w = [x; y]. Vectors are L2-normalized and phase
       canonicalized via normalize-complex-phase.

    NOTE: This pure Clojure implementation targets correctness and modest
    matrix sizes. For performance-sensitive workloads prefer a native backend.
    "
    (if (complex-matrix? A)
      (let [X (:real A) Y (:imag A)
            [n m] (matrix-shape X)
            _ (when (not= n m) (throw (ex-info "eigen-hermitian requires square matrix" {:shape [n m]})))
            ;; Real embedding M = [[X -Y][Y X]]
            top (vec (for [i (range n)] (vec (concat (nth X i) (map #(- %) (nth Y i))))))
            bottom (vec (for [i (range n)] (vec (concat (nth Y i) (nth X i)))))
            M (vec (concat top bottom))
            N (* 2 n)
            tol (* 1e-12 (inc N))
            max-it (* 10 N N)
            {:keys [eigenvalues vectors]} (jacobi-symmetric M tol max-it)
            ;; Sort embedding eigenpairs ascending
            sorted (sort-by second (map-indexed vector eigenvalues))
            collapse-tol (* 20.0 1e-12 (inc n))
            build-complex (fn [col-idx]
                            (let [w (nth vectors col-idx) ; length 2n
                                  x (subvec w 0 n)
                                  y (subvec w n N)
                                  nrm (Math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) x y)))
                                  nrm (if (pos? nrm) nrm 1.0)
                                  x' (mapv #(/ % nrm) x)
                                  y' (mapv #(/ % nrm) y)
                                  v {:real x' :imag y'}]
                              (normalize-complex-phase v 1e-14)))
            [evals evects] (loop [pairs sorted acc-e [] acc-v []]
                             (if (empty? pairs)
                               [acc-e acc-v]
                               (let [[[idx λ] & more] pairs
                                     add? (or (empty? acc-e) (> (Math/abs (- λ (last acc-e))) collapse-tol))]
                                 (if add?
                                   (recur more (conj acc-e λ) (conj acc-v (build-complex idx)))
                                   (recur more acc-e acc-v)))))]
        {:eigenvalues (mapv double evals) :eigenvectors (vec evects)})
      ;; Real symmetric path
      (let [complex? (and (map? A) (contains? A :real) (contains? A :imag))
            [n m] (if complex? [(count (:real A)) (count (first (:real A)))] (matrix-shape A))]
        (when (not= n m) (throw (ex-info "eigen-hermitian requires square matrix" {:shape [n m]})))
        (if (zero? n)
          {:eigenvalues [] :eigenvectors []}
          (let [tol (* 1e-12 (inc n))
                max-it (* 10 n n)
                {:keys [eigenvalues vectors]} (jacobi-symmetric A tol max-it)
                sorted (sort-by second (map-indexed vector eigenvalues))
                perm (map first sorted)
                evals (mapv (comp double second) sorted)
                evects (vec (map (fn [idx] (nth vectors idx)) perm))
                evects (if (= n 1) (vec (map (fn [v] [v]) evects)) evects)]
            {:eigenvalues evals :eigenvectors evects})))))

  (eigen-general [backend A]
    "General (potentially non-Hermitian) eigenvalue approximation via unshifted QR iteration.

    Parameters:
    - A: square matrix (real or complex)

    Returns map:
    {:eigenvalues [λ0 ... λ_{n-1}] :iterations k}
    Eigenvalues are approximated as the diagonal of the (quasi) upper-triangular
    form after k QR steps (no ordering guaranteed).

    Implementation notes:
    * Uses Householder QR (real or complex) each iteration.
    * No shifts or deflation -> O(k n^3) and slow for difficult spectra.
    * Stopping criterion: Frobenius norm of strict sub-diagonal < tol.
    * Complex path operates directly in complex arithmetic (no real embedding).
    * A lightweight balancing step (row/column 2-norm equilibration) is applied
      once at the beginning to improve conditioning of iteration (pedagogical).

    Limitations:
    - May converge slowly for matrices with clustered eigenvalues.
    - No Schur form extraction or eigenvector back-substitution (future work).
    - For production workloads, prefer a LAPACK-backed backend.
    "
    (let [complex? (complex-matrix? A)
          [n m] (matrix-shape A)]
      (when (not= n m) (throw (ex-info "eigen-general requires square matrix" {:shape [n m]})))
      (let [tol (* 1e-12 (inc n))
            ;; Increase iteration cap; unshifted QR can be slow for clustered spectra
            max-it (* 800 n n)
            ;; Simple balancing (single pass): scale rows & cols by sqrt(row_norm/col_norm)
            balance-real (fn [M]
                           (let [row-norms (mapv (fn [row] (Math/sqrt (reduce + (map #(* (double %) (double %)) row)))) M)
                                 cols (apply map vector M)
                                 col-norms (mapv (fn [col] (Math/sqrt (reduce + (map #(* (double %) (double %)) col)))) cols)
                                 eps 1e-14]
                             (vec (for [i (range n)]
                                    (vec (for [j (range n)]
                                           (let [ri (max eps (row-norms i)) cj (max eps (col-norms j))]
                                             (/ (get-in M [i j]) (Math/sqrt (/ ri cj))))))))))
            balance-complex (fn [M]
                              (let [R (:real M) I (:imag M)
                                    row-norms (mapv (fn [i]
                                                      (Math/sqrt (reduce + (for [j (range n)]
                                                                             (let [a (get-in R [i j]) b (get-in I [i j])] (+ (* a a) (* b b))))))) (range n))
                                    cols-r (apply map vector R)
                                    cols-i (apply map vector I)
                                    col-norms (mapv (fn [j]
                                                      (Math/sqrt (reduce + (for [i (range n)]
                                                                             (let [a (get-in R [i j]) b (get-in I [i j])] (+ (* a a) (* b b))))))) (range n))
                                    eps 1e-14]
                                {:real (vec (for [i (range n)]
                                              (vec (for [j (range n)]
                                                     (let [ri (max eps (row-norms i)) cj (max eps (col-norms j))
                                                           scale (/ 1.0 (Math/sqrt (/ ri cj)))]
                                                       (* scale (get-in R [i j])))))))
                                 :imag (vec (for [i (range n)]
                                              (vec (for [j (range n)]
                                                     (let [ri (max eps (row-norms i)) cj (max eps (col-norms j))
                                                           scale (/ 1.0 (Math/sqrt (/ ri cj)))]
                                                       (* scale (get-in I [i j])))))))}))]
        (if complex?
          ;; Complex QR iteration with fast triangular detection
          (let [A0 (balance-complex A)
                offdiag-norm-complex (fn [M]
                                       (let [R (:real M) I (:imag M)]
                                         (Math/sqrt (reduce + 0.0 (for [i (range n) j (range n) :when (> i j)]
                                                                    (let [a (double (get-in R [i j])) b (double (get-in I [i j]))]
                                                                      (+ (* a a) (* b b))))))))]
            ;; Immediate fast-path: already (quasi) upper triangular? -> diagonal eigenvalues
            (if (< (offdiag-norm-complex A0) tol)
              {:eigenvalues (mapv (fn [i] (let [ar (get-in (:real A0) [i i]) ai (get-in (:imag A0) [i i])] {:real ar :imag ai})) (range n))
               :iterations 0}
              (loop [k 0 M A0]
                (if (or (>= k max-it) (< (offdiag-norm-complex M) tol))
                  {:eigenvalues (mapv (fn [i] (let [ar (get-in (:real M) [i i]) ai (get-in (:imag M) [i i])] {:real ar :imag ai})) (range n))
                   :iterations k}
                  (let [;; trailing 2x2 for shift if n>=2
                        shift (when (>= n 2)
                                (let [i (- n 2) j (- n 1)
                                      a (get-in (:real M) [i i]) ai (get-in (:imag M) [i i])
                                      b (get-in (:real M) [i j]) bi (get-in (:imag M) [i j])
                                      c (get-in (:real M) [j i]) ci (get-in (:imag M) [j i])
                                      d (get-in (:real M) [j j]) di (get-in (:imag M) [j j])
                                      ;; eigenvalues of 2x2 complex are roots of λ^2 - (a+d)λ + (ad-bc)=0
                                      tr-r (+ a d) tr-i (+ ai di)
                                      ad-r (- (* a d) (* ai di) (* (- ai di) 0.0)) ; real(ad)
                                      ad-i (+ (* a di) (* ai d)) ; imag(ad)
                                      bc-r (- (* b c) (* bi ci))
                                      bc-i (+ (* b ci) (* bi c))
                                      det-r (- ad-r bc-r)
                                      det-i (- ad-i bc-i)
                                      ;; Compute discriminant Δ = (tr)^2 - 4 det (complex) -> approximate using real parts only for shift heuristic
                                      tr2 (+ (* tr-r tr-r) (* tr-i tr-i))
                                      det-mag (+ (* det-r det-r) (* det-i det-i))
                                      disc (Math/sqrt (Math/max 0.0 (- tr2 (* 4.0 det-mag))))
                                      μ (/ (- (+ tr-r) disc) 2.0)] ; choose smaller magnitude root approx (heuristic)
                                  μ))
                        ;; Apply real shift μ (imag ignored for stability heuristic) by subtracting μ I, perform QR, add back
                        μ (double (or shift 0.0))
                        M-shift (if (zero? μ) M
                                    {:real (vec (for [i (range n)]
                                                  (vec (for [j (range n)]
                                                         (let [val (get-in (:real M) [i j])]
                                                           (if (= i j) (- val μ) val))))))
                                     :imag (:imag M)})
                        {:keys [Q R]} (proto/qr-decomposition backend M-shift)
                        M-next (let [RQ (complex-mul R Q)]
                                 (if (zero? μ) RQ
                                     ;; add shift back: RQ + μ I
                                     (let [Rr (:real RQ) Ri (:imag RQ)]
                                       {:real (vec (for [i (range n)]
                                                     (vec (for [j (range n)]
                                                            (let [val (get-in Rr [i j])]
                                                              (if (= i j) (+ val μ) val))))))
                                        :imag Ri})))]
                    (recur (inc k) M-next))))))
          ;; Real QR iteration with simple Wilkinson shift
          (let [Araw (mapv vec A)
                offdiag-norm-real (fn [M]
                                    (Math/sqrt (reduce + 0.0 (for [i (range n) j (range n) :when (> i j)]
                                                               (let [v (double (get-in M [i j]))] (* v v))))))]
            (if (< (offdiag-norm-real Araw) tol)
              {:eigenvalues (mapv #(get-in Araw [% %]) (range n)) :iterations 0}
              (let [A0 (balance-real Araw)]
                (loop [k 0 M A0]
                  (if (or (>= k max-it) (< (offdiag-norm-real M) tol))
                    {:eigenvalues (mapv #(get-in M [% %]) (range n)) :iterations k}
                    (let [μ (if (>= n 2)
                              (let [i (- n 2) j (- n 1)
                                    a (get-in M [i i]) b (get-in M [i j])
                                    c (get-in M [j i]) d (get-in M [j j])
                                    tr (+ a d)
                                    det (- (* a d) (* b c))
                                    disc (Math/sqrt (Math/max 0.0 (- (* tr tr) (* 4.0 det))))]
                                (/ (- tr disc) 2.0))
                              0.0)
                          M-shift (if (zero? μ) M (vec (for [i (range n)] (vec (for [j (range n)] (if (= i j) (- (get-in M [i j]) μ) (get-in M [i j])))))))
                          {:keys [Q R]} (proto/qr-decomposition backend M-shift)
                          Qn (vec (map #(subvec % 0 n) (subvec Q 0 n)))
                          Rn (vec (map #(subvec % 0 n) (subvec R 0 n)))
                          RQ (real-mul Rn Qn)
                          M-next (if (zero? μ) RQ (vec (for [i (range n)] (vec (for [j (range n)] (if (= i j) (+ (get-in RQ [i j]) μ) (get-in RQ [i j])))))))]
                      (recur (inc k) M-next)))))))))))

  (svd [_ A]
    "Singular Value Decomposition A = U Σ Vᴴ supporting:
     * Real and complex (SoA) matrices
     * Rectangular shapes (m×n)
     * Any size (no artificial size cap – algorithm is O(m n^2) dominated by eigen solve)

    Returned map keys:
      :U   (m×m real or complex unitary/orthogonal matrix)
      :S   vector of singular values σ₀ ≥ σ₁ ≥ … ≥ σ_{min(m,n)-1}
      :Vt  Vᵀ (real) or Vᴴ (complex) – rows are right singular vectors
      :V†  alias of :Vt for convenience in quantum code (unitary adjoint)

    Implementation strategy (classic):
      1. Form B = AᵀA (real) or B = AᴴA (complex Hermitian, PSD).
      2. Hermitian eigendecomposition B v_i = λ_i v_i via proto/eigen-hermitian.
      3. Singular values σ_i = sqrt(max(λ_i,0)). Order descending.
      4. Right singular vectors = v_i.
      5. Left singular vectors u_i = (1/σ_i) A v_i for σ_i > tol; for σ_i ≈ 0, extend
         an orthonormal basis via Gram–Schmidt on canonical basis vectors.
      6. Orthonormal completion for U (if m>k) and V (if n>k) when rank < k.

    Notes:
      * Uses backend tolerance for rank decisions.
      * Falls back to simple Gram–Schmidt; performance tuned backends should
        supply specialized SVD implementations.
      * For complex vectors we maintain SoA {:real [...], :imag [...]} columns.
      * Economy (thin) SVD could be derived by taking first k columns of U and V.
    "
    (let [complex? (complex-matrix? A)
          tol (double default-tolerance)]
      (if complex?
        ;; Complex SVD path --------------------------------------------------
        (let [Ar (:real A) Ai (:imag A)
              m (count Ar) n (count (first Ar))
              ;; Build AᴴA = (conj-transpose A) * A
              Ah {:real (real-transpose Ar) :imag (real-scale (real-transpose Ai) -1.0)}
              AhA (complex-mul Ah {:real Ar :imag Ai})
              {:keys [eigenvalues eigenvectors]} (proto/eigen-hermitian _ AhA)
              ;; eigenvalues ascending per contract; reverse for descending singular values
              pairs (reverse (map vector eigenvalues eigenvectors))
              ;; Process singular triplets
              sv-pairs (map (fn [[λ v]] [(Math/sqrt (Math/max 0.0 (double λ))) v]) pairs)
              ;; Filter numerical noise ordering & produce descending order by σ
              sv-pairs (sort-by (fn [[s _]] (- s)) sv-pairs)
              k (min m n)
              sv-pairs (take k sv-pairs)
              singular-values (mapv first sv-pairs)
              V-cols (map second sv-pairs)
              ;; Helper: complex matvec already available (complex-matvec)
              compute-u (fn [sigma v]
                          (if (> sigma tol)
                            (let [u (complex-matvec {:real Ar :imag Ai} v)
                                  norm-sigma sigma
                                  ur (:real u) ui (:imag u)
                                  u-norm (Math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) ur ui)))
                                  ;; divide by sigma (not u-norm) per definition: u = Av / σ
                                  scale (/ 1.0 norm-sigma)]
                              {:real (mapv #(* scale %) ur)
                               :imag (mapv #(* scale %) ui)})
                            ;; placeholder zero vector; will be replaced in orthonormal completion
                            {:real (vec (repeat m 0.0)) :imag (vec (repeat m 0.0))}))
              U-cols (map (fn [[s v]] (compute-u s v)) sv-pairs)
              ;; Gram–Schmidt for complex vectors
              inner-c (fn [x y] (complex-inner x y))
              sub-c (fn [x y]
                      {:real (mapv - (:real x) (:real y))
                       :imag (mapv - (:imag x) (:imag y))})
              scale-c (fn [x alpha]
                        {:real (mapv #(* alpha %) (:real x))
                         :imag (mapv #(* alpha %) (:imag x))})
              mult-cv (fn [c v]
                        (let [ar (:real c) ai (:imag c) vr (:real v) vi (:imag v)]
                          {:real (mapv (fn [r i] (- (* ar r) (* ai i))) vr vi)
                           :imag (mapv (fn [r i] (+ (* ar i) (* ai r))) vr vi)}))
              norm-c (fn [x]
                       (Math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) (:real x) (:imag x)))))
              normalize-c (fn [x]
                            (let [nrm (norm-c x)]
                              (if (pos? nrm)
                                (scale-c x (/ 1.0 nrm))
                                x)))
              orthonormalize (fn [cols]
                               (reduce (fn [acc v]
                                         (let [v1 (reduce (fn [vv u]
                                                            (let [ip (inner-c u vv)]
                                                              (sub-c vv (mult-cv ip u))))
                                                          v acc)
                                               v-n (normalize-c v1)]
                                           (conj acc v-n))) [] cols))
              U-cols (orthonormalize U-cols) ; re-orthonormalize in case of numerical issues
              ;; Orthonormal completion for rank deficiency
              rank (count (filter #(> % tol) singular-values))
              complete-basis (fn [existing dim]
                               (loop [basis existing i 0]
                                 (if (= (count basis) dim)
                                   basis
                                   (if (>= i dim)
                                     basis
                                     (let [e {:real (vec (for [k (range dim)] (if (= k i) 1.0 0.0)))
                                              :imag (vec (repeat dim 0.0))}
                                           v1 (reduce (fn [vv u]
                                                        (let [ip (inner-c u vv)]
                                                          (sub-c vv (mult-cv ip u)))) e basis)
                                           nrm (norm-c v1)]
                                       (if (> nrm (* 10 tol))
                                         (recur (conj basis (scale-c v1 (/ 1.0 nrm))) (inc i))
                                         (recur basis (inc i))))))))
              U-full (complete-basis U-cols m)
              V-full (complete-basis V-cols n)
              Vh {:real (real-transpose (mapv :real V-full))
                  :imag (real-scale (real-transpose (mapv :imag V-full)) -1.0)}
              U-mat {:real (vec (apply map vector (map :real U-full)))
                     :imag (vec (apply map vector (map :imag U-full)))}]
          {:U U-mat :S singular-values :Vt Vh :V† Vh})
        ;; Real SVD path -----------------------------------------------------
        (let [A (mapv vec A)
              [m n] (matrix-shape A)
              At (real-transpose A)
              AtA (real-mul At A)
              {:keys [eigenvalues eigenvectors]} (proto/eigen-hermitian _ AtA)
              pairs (reverse (map vector eigenvalues eigenvectors))
              sv-pairs (map (fn [[λ v]] [(Math/sqrt (Math/max 0.0 (double λ))) v]) pairs)
              sv-pairs (sort-by (fn [[s _]] (- s)) sv-pairs)
              k (min m n)
              sv-pairs (take k sv-pairs)
              singular-values (mapv first sv-pairs)
              V-cols (map second sv-pairs)
              ;; Left singular vectors
              compute-u (fn [sigma v]
                          (if (> sigma tol)
                            (let [Av (mapv (fn [row] (reduce + (map * row v))) A)
                                  scale (/ 1.0 sigma)]
                              (mapv #(* scale %) Av))
                            (vec (repeat m 0.0))))
              U-cols (map (fn [[s v]] (compute-u s v)) sv-pairs)
              ;; Gram–Schmidt real
              inner-r (fn [x y] (reduce + (map * x y)))
              norm-r (fn [x] (Math/sqrt (reduce + (map #(* % %) x))))
              gs (fn [cols]
                   (reduce (fn [acc v]
                             (let [v1 (reduce (fn [vv u]
                                                (let [ip (inner-r u vv)]
                                                  (mapv - vv (mapv #(* ip %) u)))) v acc)
                                   nrm (norm-r v1)
                                   v2 (if (> nrm 0.0) (mapv #(/ % nrm) v1) v1)]
                               (conj acc v2))) [] cols))
              U-cols (gs U-cols)
              rank (count (filter #(> % tol) singular-values))
              complete (fn [existing dim]
                         (loop [basis existing i 0]
                           (if (= (count basis) dim)
                             basis
                             (if (>= i dim)
                               basis
                               (let [e (vec (for [k (range dim)] (if (= k i) 1.0 0.0)))
                                     v1 (reduce (fn [vv u]
                                                  (let [ip (inner-r u vv)]
                                                    (mapv - vv (mapv #(* ip %) u)))) e basis)
                                     nrm (norm-r v1)]
                                 (if (> nrm (* 10 tol))
                                   (recur (conj basis (mapv #(/ % nrm) v1)) (inc i))
                                   (recur basis (inc i))))))))
              U-full (complete U-cols m)
              V-full (complete V-cols n)
              U-mat (vec (apply map vector U-full))  ; columns -> rows transpose to matrix
              Vt (vec (apply map vector V-full))]
          {:U U-mat :S singular-values :Vt Vt :V† Vt}))))

  (lu-decomposition [_ A]
    (if (complex-matrix? A)
      ;; Complex LU with partial pivoting
      (let [Ar (mapv vec (:real A)) Ai (mapv vec (:imag A))
            n (count Ar)
            P0 (vec (range n))
            Lr (vec (repeat n (vec (repeat n 0.0))))
            Li (vec (repeat n (vec (repeat n 0.0))))]
        (loop [k 0 Ar Ar Ai Ai Lr Lr Li Li P P0]
          (if (= k n)
            {:P P :L {:real (vec (map-indexed (fn [i row] (assoc row i 1.0)) Lr)) :imag Li}
             :U {:real Ar :imag Ai}}
            (let [pivot-row (->> (range k n) (apply max-key (fn [r]
                                                              (let [pr (get-in Ar [r k]) pi (get-in Ai [r k])]
                                                                (+ (* pr pr) (* pi pi))))))
                  swap-row (fn [M] (if (not= pivot-row k) (-> M (assoc k (M pivot-row)) (assoc pivot-row (M k))) M))
                  Ar (swap-row Ar) Ai (swap-row Ai)
                  Lr (if (not= pivot-row k)
                       (assoc Lr pivot-row (Lr k) k (Lr pivot-row)) Lr)
                  Li (if (not= pivot-row k)
                       (assoc Li pivot-row (Li k) k (Li pivot-row)) Li)
                  P (if (not= pivot-row k) (-> P (assoc k (P pivot-row)) (assoc pivot-row (P k))) P)
                  pr (get-in Ar [k k]) pi (get-in Ai [k k])
                  denom (+ (* pr pr) (* pi pi))]
              (when (zero? denom) (throw (ex-info "Singular complex matrix in LU" {:k k})))
              (let [step (fn [[Ar Ai Lr Li] i]
                           (let [ur (get-in Ar [i k]) ui (get-in Ai [i k])
                                 fr (/ (+ (* ur pr) (* ui pi)) denom)
                                 fi (/ (- (* ui pr) (* ur pi)) denom)
                                 ;; store in L (below diag)
                                 Lr (assoc-in Lr [i k] fr)
                                 Li (assoc-in Li [i k] fi)
                                 update-row (fn [Ar Ai j]
                                              (let [akr (get-in Ar [k j]) aki (get-in Ai [k j])
                                                    aij-r (get-in Ar [i j]) aij-i (get-in Ai [i j])
                                                    prod-r (- (* fr akr) (* fi aki))
                                                    prod-i (+ (* fr aki) (* fi akr))]
                                                [(assoc-in Ar [i j] (- aij-r prod-r))
                                                 (assoc-in Ai [i j] (- aij-i prod-i))]))
                                 [Ar Ai] (reduce (fn [[Ar Ai] j] (update-row Ar Ai j)) [Ar Ai] (range k n))]
                             [Ar Ai Lr Li]))
                    [Ar Ai Lr Li] (reduce step [Ar Ai Lr Li] (range (inc k) n))]
                (recur (inc k) Ar Ai Lr Li P))))))
      (let [[n n2] (matrix-shape A)]
        (when (not= n n2) (throw (ex-info "LU requires square matrix" {:shape [n n2]})))
        (let [A0 (mapv vec A) P0 (vec (range n)) L0 (vec (repeat n (vec (repeat n 0.0))))]
          (loop [k 0 A A0 L L0 P P0]
            (if (= k n)
              {:P P :L (vec (map-indexed (fn [i row] (assoc row i 1.0)) L)) :U A}
              (let [pivot-row (->> (range k n) (apply max-key (fn [r] (Math/abs (double (get-in A [r k]))))))
                    A (if (not= pivot-row k) (-> A (assoc k (A pivot-row)) (assoc pivot-row (A k))) A)
                    P (if (not= pivot-row k) (-> P (assoc k (P pivot-row)) (assoc pivot-row (P k))) P)
                    akk (double (get-in A [k k]))]
                (when (zero? akk) (throw (ex-info "Singular matrix in LU" {:k k})))
                (let [[A L] (reduce (fn [[A L] i]
                                      (let [factor (/ (double (get-in A [i k])) akk)
                                            row (mapv - (A i) (mapv #(* factor %) (A k)))]
                                        [(assoc A i row) (assoc-in L [i k] factor)]))
                                    [A L] (range (inc k) n))]
                  (recur (inc k) A L P)))))))))

  (qr-decomposition [_ A]
    (if (complex-matrix? A)
      (let [Ar (:real A) Ai (:imag A)
            [m n] (matrix-shape A)
            get-col (fn [Mr Mi j] {:real (mapv #(get-in Mr [% j]) (range m)) :imag (mapv #(get-in Mi [% j]) (range m))})]
        (loop [j 0 Q [] Rr (vec (for [_ (range n)] (vec (repeat n 0.0)))) Ri (vec (for [_ (range n)] (vec (repeat n 0.0))))]
          (if (= j n)
            {:Q {:real (vec (apply mapv vector (map :real Q))) :imag (vec (apply mapv vector (map :imag Q)))}
             :R {:real (vec (mapv vec Rr)) :imag (vec (mapv vec Ri))}}
            (let [v0 (get-col Ar Ai j)
                  [v Rr Ri] (reduce (fn [[vv Rr Ri] i]
                                      (let [qi (nth Q i)
                                            ;; qr/qi-norm2 omitted (not needed explicitly)
                                            ;; Use complex inner product (qi^H vv)
                                            pr (reduce + (map (fn [a b c d] (+ (* a b) (* c d))) (:real qi) (:real vv) (:imag qi) (:imag vv)))
                                            pi (reduce + (map (fn [a b c d] (- (* a d) (* c b))) (:real qi) (:real vv) (:imag qi) (:imag vv)))
                                            ;; vv = vv - qi * (pr + i pi)
                                            vr' (mapv (fn [vr qjr qji]
                                                        (- vr (- (* pr qjr) (* pi qji)))) (:real vv) (:real qi) (:imag qi))
                                            vi' (mapv (fn [vi qjr qji]
                                                        (- vi (+ (* pr qji) (* pi qjr)))) (:imag vv) (:real qi) (:imag qi))
                                            Rr (assoc-in Rr [i j] pr) Ri (assoc-in Ri [i j] pi)]
                                        [{:real vr' :imag vi'} Rr Ri]))
                                    [v0 Rr Ri] (range j))
                  rjj (Math/sqrt (reduce + (map #(+ (* % %) 0.0) (:real v))))
                  qj (if (pos? rjj) {:real (mapv #(/ % rjj) (:real v)) :imag (mapv #(/ % rjj) (:imag v))}
                         {:real (vec (repeat m 0.0)) :imag (vec (repeat m 0.0))})
                  Rr (assoc-in Rr [j j] rjj)
                  Q (conj Q qj)]
              (recur (inc j) Q Rr Ri)))))
      (householder-qr-real A)))

  (cholesky-decomposition [_ A]
    (if (complex-matrix? A)
      (let [Ar (:real A) Ai (:imag A) n (count Ar)]
        (loop [i 0 Lr (vec (repeat n (vec (repeat n 0.0)))) Li (vec (repeat n (vec (repeat n 0.0))))]
          (if (= i n)
            {:L {:real Lr :imag Li}}
            (let [[Lr Li]
                  (loop [j 0 Lr Lr Li Li]
                    (if (> j i) [Lr Li]
                        (let [sum-r (reduce + (for [k (range j)]
                                                (let [lrik (get-in Lr [i k]) liik (get-in Li [i k]) lrjk (get-in Lr [j k]) lijK (get-in Li [j k])]
                                                  (- (* lrik lrjk) (* liik lijK)))))
                              sum-i (reduce + (for [k (range j)]
                                                (let [lrik (get-in Lr [i k]) liik (get-in Li [i k]) lrjk (get-in Lr [j k]) lijK (get-in Li [j k])]
                                                  (+ (* lrik lijK) (* liik lrjk)))))
                              arij (get-in Ar [i j]) aiij (get-in Ai [i j])
                              diff-r (- arij sum-r) diff-i (- aiij sum-i)]
                          (if (= i j)
                            (do
                              (when (> (Math/abs diff-i) 1e-12) (throw (ex-info "Hermitian diag not real" {:i i :imag diff-i})))
                              (when (neg? diff-r) (throw (ex-info "Matrix not HPD" {:i i :value diff-r})))
                              (let [val (Math/sqrt diff-r)]
                                (recur (inc j) (assoc-in Lr [i i] val) Li)))
                            (let [lrjj (get-in Lr [j j])
                                  fr (/ diff-r lrjj) fi (/ diff-i lrjj)
                                  Lr (assoc-in Lr [i j] fr)
                                  Li (assoc-in Li [i j] fi)]
                              (recur (inc j) Lr Li))))))]
              (recur (inc i) Lr Li)))))
      (let [[n n2] (matrix-shape A)]
        (when (not= n n2) (throw (ex-info "Cholesky requires square" {:shape [n n2]})))
        (loop [i 0 L (vec (repeat n (vec (repeat n 0.0))))]
          (if (= i n)
            {:L L}
            (let [L (loop [j 0 L L]
                      (if (> j i) L
                          (let [sum (reduce + (for [k (range j)] (* (get-in L [i k]) (get-in L [j k]))))
                                val (double (get-in A [i j]))
                                lij (if (= i j)
                                      (let [d (- val sum)]
                                        (when (neg? d) (throw (ex-info "Matrix not SPD" {:i i :j j :value d})))
                                        (Math/sqrt d))
                                      (/ (- val sum) (double (get-in L [j j]))))
                                row (assoc (get L i) j lij)]
                            (recur (inc j) (assoc L i row)))))]
              (recur (inc i) L))))))))

;;
;; MatrixFunctions placeholders
;;
(extend-protocol proto/MatrixFunctions
  ClojureMathBackend
  (matrix-exp [backend A]
    (if (complex-matrix? A)
      (let [X (:real A) Y (:imag A)
            n (count X)]
        (cond
          ;; 1x1 complex scalar
          (and (= 1 n) (= 1 (count (first X))))
          (let [a (double (get-in X [0 0])) b (double (get-in Y [0 0])) ea (Math/exp a)]
            {:real [[(* ea (Math/cos b))]] :imag [[(* ea (Math/sin b))]]})
          ;; Diagonal complex matrix: exp acts element-wise
          (every? true? (for [i (range n) j (range n) :when (not= i j)]
                          (and (zero? (double (get-in X [i j]))) (zero? (double (get-in Y [i j]))))))
          (let [R (vec (for [i (range n)]
                         (vec (for [j (range n)]
                                (if (= i j)
                                  (let [a (double (get-in X [i i])) b (double (get-in Y [i i])) ea (Math/exp a)]
                                    (* ea (Math/cos b)))
                                  0.0)))))
                I (vec (for [i (range n)]
                         (vec (for [j (range n)]
                                (if (= i j)
                                  (let [a (double (get-in X [i i])) b (double (get-in Y [i i])) ea (Math/exp a)]
                                    (* ea (Math/sin b)))
                                  0.0)))))]
            {:real R :imag I})
          :else
          ;; General complex: embed A = X + iY into real block [[X -Y][Y X]] and exponentiate
          (let [Z (vec (for [i (range n)] (vec (concat (nth X i) (map #(- (double %)) (nth Y i))))))
                Z2 (vec (for [i (range n)] (vec (concat (nth Y i) (nth X i)))))
                B (into [] (concat Z Z2))
                E (proto/matrix-exp backend B)
                Er (vec (for [i (range n)] (subvec (E i) 0 n)))
                Ei (vec (for [i (range n)] (subvec (E (+ i n)) 0 n)))]
            {:real Er :imag Ei})))
      (let [[n _] (matrix-shape A)
            I (identity-matrix n)
            tol 1e-13
            ;; Try nilpotent fast path first (strictly triangular, etc.)
            nilpotent (nilpotent-matrix-exp A tol)]
        (cond
          nilpotent (:exp nilpotent)
          (hermitian-real? A 1e-12)
          ;; Eigen decomposition based exp for symmetric matrices
          (let [{:keys [eigenvalues eigenvectors]} (proto/eigen-hermitian backend A)]
            (reduce (fn [acc [lambda v]]
                      (let [e (Math/exp lambda)
                            outer (vec (for [a v] (vec (for [b v] (* e a b)))))]
                        (real-add acc outer)))
                    (vec (repeat n (vec (repeat n 0.0))))
                    (map vector eigenvalues eigenvectors)))
          :else
          ;; General matrix: scaling & squaring Padé (6,6)
          (let [norm (real-one-norm A)
                theta 3.0
                ;; compute s such that norm/2^s <= theta
                s (let [ratio (/ norm theta)]
                    (if (<= ratio 1.0) 0 (int (Math/ceil (/ (Math/log ratio) (Math/log 2.0))))))
                s (if (neg? s) 0 s)
                A1 (if (pos? s) (real-scale A (/ 1.0 (Math/pow 2.0 s))) A)
                A2 (real-mul A1 A1)
                A4 (real-mul A2 A2)
                A6 (real-mul A4 A2)
                c0 1.0 c1 1.0 c2 (/ 1.0 2.0) c3 (/ 1.0 6.0) c4 (/ 1.0 24.0) c5 (/ 1.0 120.0) c6 (/ 1.0 720.0)
                U (real-mul A1 (real-add (real-scale I c1)
                                         (real-add (real-scale A2 c3)
                                                   (real-scale A4 c5))))
                V (-> (real-scale I c0)
                      (real-add (real-scale A2 c2))
                      (real-add (real-scale A4 c4))
                      (real-add (real-scale A6 c6)))
                V+U (real-add V U)
                V-U (real-sub V U)
                Vinv (proto/inverse backend V-U)
                R (real-mul Vinv V+U)
                R (loop [i 0 M R] (if (= i s) M (recur (inc i) (real-mul M M))))]
            R)))))

  (matrix-log [backend A]
    ;; Matrix logarithm implementation.
    ;; Real path: requires real symmetric positive definite (SPD) matrix.
    ;; Complex path: supports Hermitian positive definite complex matrix.
    (if (complex-matrix? A)
      (let [Ar (:real A) Ai (:imag A) [n _] (matrix-shape A)]
        (when (zero? n) {:real [] :imag []})
        (when (not (hermitian-complex? A 1e-10)) (throw (ex-info "matrix-log implemented only for Hermitian positive definite complex matrices" {:matrix A})))
        ;; Use eigen-decomposition A = V Λ V^H, then log(A) = V log(Λ) V^H
        (let [{:keys [eigenvalues eigenvectors]} (proto/eigen-hermitian backend A)]
          (doseq [l eigenvalues]
            (when (<= l 0.0) (throw (ex-info "matrix-log requires positive eigenvalues" {:lambda l}))))
          (let [log-L (map #(Math/log %) eigenvalues)
                ;; Build log(A) = sum_i (log λ_i) v_i v_i^H
                {:keys [real imag] :as acc}
                (reduce (fn [{:keys [real imag]} [lv v]]
                          (let [vr (:real v) vi (:imag v)
                                ;; outer hermitian rank-1: v v^H
                                add-r (vec (for [i (range n)]
                                             (vec (for [j (range n)]
                                                    (* lv (+ (* (vr i) (vr j)) (* (vi i) (vi j))))))))
                                add-i (vec (for [i (range n)]
                                             (vec (for [j (range n)]
                                                    (* lv (- (* (vi i) (vr j)) (* (vr i) (vi j))))))))]
                            {:real (real-add real add-r)
                             :imag (real-add imag add-i)}))
                        {:real (vec (repeat n (vec (repeat n 0.0))))
                         :imag (vec (repeat n (vec (repeat n 0.0))))}
                        (map vector log-L eigenvectors))]
            {:real real :imag imag})))
      (let [[n _] (matrix-shape A)]
        (when (zero? n) [])
        (when (not (hermitian-real? A 1e-10)) (throw (ex-info "matrix-log implemented only for real symmetric positive definite" {:matrix A})))
        (let [{:keys [eigenvalues eigenvectors]} (proto/eigen-hermitian backend A)]
          (doseq [l eigenvalues]
            (when (<= l 0.0) (throw (ex-info "matrix-log requires positive eigenvalues" {:lambda l}))))
          (reduce (fn [acc [lambda v]]
                    (let [lv (Math/log lambda)
                          outer (vec (for [a v] (vec (for [b v] (* lv a b)))))]
                      (real-add acc outer)))
                  (vec (repeat n (vec (repeat n 0.0))))
                  (map vector eigenvalues eigenvectors))))))

  (matrix-sqrt [backend A]
    ;; Matrix square root.
    ;; Real path: PSD matrix via Denman–Beavers iteration.
    ;; Complex path: Hermitian positive semidefinite complex matrix via complex Denman–Beavers.
    (if (complex-matrix? A)
      (let [[n n2] (matrix-shape A)]
        (when (not= n n2) (throw (ex-info "matrix-sqrt requires square" {:shape [n n2]})))
        (when (not (hermitian-complex? A 1e-10)) (throw (ex-info "Matrix square root requires positive semidefinite matrix" {:matrix A :reason :not-hermitian})))
        (when (not (proto/positive-semidefinite? backend A)) (throw (ex-info "Matrix square root requires positive semidefinite matrix" {:matrix A :reason :not-psd})))
        (let [I {:real (identity-matrix n) :imag (vec (repeat n (vec (repeat n 0.0))))}
              tol 1e-10 max-it 60]
          (loop [k 0 Y A Z I]
            (let [Zinv (proto/inverse backend Z)
                  Yinv (proto/inverse backend Y)
                  Ynext (complex-scale (complex-add Y Zinv) 0.5)
                  Znext (complex-scale (complex-add Z Yinv) 0.5)
                  diff (let [YY (complex-mul Ynext Ynext)
                             D (complex-sub YY A)
                             nr (Math/sqrt (reduce + (for [i (range n) j (range n)]
                                                       (let [dr (get-in (:real D) [i j]) di (get-in (:imag D) [i j])]
                                                         (+ (* dr dr) (* di di))))))]
                         nr)]
              (if (or (< diff tol) (>= k max-it))
                Ynext
                (recur (inc k) Ynext Znext))))))
      (let [[n n2] (matrix-shape A)]
        (when (not= n n2) (throw (ex-info "matrix-sqrt requires square" {:shape [n n2]})))
        (when (not (hermitian-real? A 1e-10)) (throw (ex-info "Matrix square root requires positive semidefinite matrix" {:matrix A :reason :not-symmetric})))
        (when (not (proto/positive-semidefinite? backend A)) (throw (ex-info "Matrix square root requires positive semidefinite matrix" {:matrix A :reason :not-psd})))
        (let [I (identity-matrix n)
              tol 1e-10 max-it 50]
          (loop [k 0 Y A Z I]
            (let [Zinv (proto/inverse backend Z)
                  Yinv (proto/inverse backend Y)
                  Ynext (real-scale (real-add Y Zinv) 0.5)
                  Znext (real-scale (real-add Z Yinv) 0.5)
                  diff (real-frobenius-norm (real-sub (real-mul Ynext Ynext) A))]
              (if (or (< diff tol) (>= k max-it))
                Ynext
                (recur (inc k) Ynext Znext)))))))))

;;
;; MatrixAnalysis
;;
;; MatrixAnalysis protocol currently uses spectral-norm via MatrixAlgebra implementation.
;; Other analysis functions could be added here later.
(extend-protocol proto/MatrixAnalysis
  ClojureMathBackend

  (spectral-norm [_ A]
    (if (complex-matrix? A)
      (spectral-norm-complex A)
      (spectral-norm-real A)))

  (condition-number [backend A]
    (let [normA (proto/spectral-norm backend A)
          invA (try (proto/inverse backend A) (catch Exception _ nil))]
      (if invA
        (let [normInv (proto/spectral-norm backend invA)]
          (* normA normInv))
        Double/POSITIVE_INFINITY))))

;;
;; QuantumStateOps
;;
(extend-protocol proto/QuantumStateOps
  ClojureMathBackend
  (state-normalize [b state]
    (if (complex-vector? state)
      (let [nrm (proto/norm2 b state)
            s (if (pos? nrm) (/ 1.0 nrm) 1.0)]
        {:real (mapv #(* s %) (:real state))
         :imag (mapv #(* s %) (:imag state))})
      (let [nrm (Math/sqrt (reduce + (map #(* (double %) (double %)) state)))
            s (if (pos? nrm) (/ 1.0 nrm) 1.0)]
        (mapv #(* s (double %)) state))))

  (projector-from-state [b psi]
    (cond (complex-vector? psi) (proto/outer-product b psi psi)
          (vector? psi) (proto/outer-product b psi psi)
          :else (throw (ex-info "Unsupported state" {:psi psi}))))

  (density-matrix [b psi] (proto/projector-from-state b psi))

  (trace-one?
    ([b rho] (proto/trace-one? b rho (tolerance* b)))
    ([b rho eps]
     (let [tr (proto/trace b rho)
           eps (double (or eps default-tolerance))]
       (if (complex-scalar? tr)
         (and (< (Math/abs (- (:real tr) 1.0)) eps)
              (< (Math/abs (:imag tr)) eps))
         (< (Math/abs (- (double tr) 1.0)) eps))))))

;;
;; Factory
;;
(defn make-backend
  "Create a new Clojure math backend. Options:
   :tolerance  numeric tolerance used by predicates (default 1e-12)
   :config     arbitrary config map."
  ([] (->ClojureMathBackend default-tolerance {:tolerance default-tolerance}))
  ([{:keys [tolerance] :as opts}]
   (->ClojureMathBackend (or tolerance default-tolerance)
                         (merge {:tolerance (or tolerance default-tolerance)} (dissoc opts :tolerance)))))

;; End of backend implementation
 
