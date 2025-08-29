(ns org.soulspace.qclojure.domain.math.clojure.complex-linear-algebra
  (:require
   [clojure.math :as math]))

;;;
;;; Configuration and utilities
;;;
(def ^:const ^double default-tolerance 1.0e-12)

;;;
;;; Complex number utilities and predicates
;;;
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


;;;
;;; Matrix algebra helper functions
;;;
(defn- identity-matrix [n]
  (vec (for [i (range n)] (vec (for [j (range n)] (double (if (= i j) 1.0 0.0)))))))

(defn matrix-shape [A]
  (if (complex-matrix? A)
    [(count (:real A)) (count (first (:real A)))]
    [(count A) (count (first A))]))

(defn close-matrices? [A B tol]
  (let [[r c] (matrix-shape A)]
    (every? true?
            (for [i (range r) j (range c)]
              (< (abs (double (- (if (complex-matrix? A)
                                   (get-in (:real A) [i j])
                                   (get-in A [i j]))
                                 (if (complex-matrix? B)
                                   (get-in (:real B) [i j])
                                   (get-in B [i j]))))) tol)))))

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

(defn- real-transpose [A] (vec (apply mapv vector A)))

(defn- real-kronecker [A B]
  (let [[ar ac] (matrix-shape A) [br bc] (matrix-shape B)]
    (vec (for [i (range ar)
               bi (range br)]
           (vec (for [j (range ac)
                      bj (range bc)]
                  (* (double (get-in A [i j])) (double (get-in B [bi bj])))))))))


;;
;; Complex (SoA) matrix operations
;;
(defn matrix-add [A B]
  {:real (real-add (:real A) (:real B))
   :imag (real-add (:imag A) (:imag B))})

(defn matrix-subtract [A B]
  {:real (real-sub (:real A) (:real B))
   :imag (real-sub (:imag A) (:imag B))})

(defn matrix-scale [A a]
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

(defn matrix-multiply [A B]
  (let [Ar (:real A) Ai (:imag A) Br (:real B) Bi (:imag B)
        AC (real-mul Ar Br)
        BD (real-mul Ai Bi)
        AD (real-mul Ar Bi)
        BC (real-mul Ai Br)]
    {:real (real-sub AC BD)
     :imag (real-add AD BC)}))

(defn matrix-vector-product [A x]
  (let [Ar (:real A) Ai (:imag A) xr (:real x) xi (:imag x)
        mul-r (real-mul Ar (mapv vector xr)) ; treat vector as col matrix
        mul-i (real-mul Ai (mapv vector xi))
        mul-r2 (real-mul Ar (mapv vector xi))
        mul-i2 (real-mul Ai (mapv vector xr))]
    {:real (mapv #(- %1 %2) (mapv first mul-r) (mapv first mul-i))
     :imag (mapv #(+ %1 %2) (mapv first mul-r2) (mapv first mul-i2))}))

(defn hadamard-product [A B]
  (let [Ar (:real A) Ai (:imag A) Br (:real B) Bi (:imag B)
        Cr (mapv (fn [ra ia rb ib]
                   (mapv (fn [a i b j] (- (* a b) (* i j))) ra ia rb ib)) Ar Ai Br Bi)
        Ci (mapv (fn [ra ia rb ib]
                   (mapv (fn [a i b j] (+ (* a j) (* i b))) ra ia rb ib)) Ar Ai Br Bi)]
    {:real Cr :imag Ci}))

(defn kronecker-product [A B]
  (let [Ar (:real A) Ai (:imag A) Br (:real B) Bi (:imag B)
        RR (real-kronecker Ar Br)
        II (real-kronecker Ai Bi)
        RI (real-kronecker Ar Bi)
        IR (real-kronecker Ai Br)]
    {:real (real-sub RR II)
     :imag (real-add RI IR)}))

(defn transpose [A]
  {:real (real-transpose (:real A))
   :imag (real-transpose (:imag A))})

(defn conjugate-transpose [A]
  {:real (real-transpose (:real A))
   :imag (real-scale (real-transpose (:imag A)) -1.0)})

(defn inner-product [x y]
  (let [xr (:real x) xi (:imag x) yr (:real y) yi (:imag y)
        re (reduce + (map (fn [a b c d] (+ (* a b) (* c d))) xr yr xi yi))
        im (reduce + (map (fn [a b c d] (- (* a d) (* c b))) xr yr xi yi))]
    (make-complex re im)))

(defn hermitian? [A tol]
  (let [Ar (:real A) Ai (:imag A) n (count Ar)]
    (and (= n (count (first Ar))) ; square
         (every? true?
                 (for [i (range n) j (range i n)]
                   (let [aij-r (get-in Ar [i j]) aij-i (get-in Ai [i j])
                         aji-r (get-in Ar [j i]) aji-i (get-in Ai [j i])]
                     (and (< (abs (double (- aij-r aji-r))) tol)
                          (< (abs (double (+ aij-i aji-i))) tol))))))))

;; Complex Gaussian elimination helpers
;;
;; These implement partial pivot Gaussian elimination for a single RHS vector
;; and Gauss-Jordan inversion for complex matrices represented in SoA form.
;; They are intentionally straightforward (no blocking / BLAS) and target
;; small to medium matrix sizes.

(defn- forward-elimination
  "Forward elimination (partial pivot) for complex A x = b.
  Ar/Ai: matrix parts, br/bi: RHS parts. Returns [Ar Ai br bi] in row echelon form."
  [Ar Ai br bi]
  (let [n (count Ar)]
    (loop [k 0 Ar Ar Ai Ai br br bi bi]
      (if (= k n)
        [Ar Ai br bi]
        (let [pivot (apply max-key #(math/hypot (get-in Ar [% k]) (get-in Ai [% k])) (range k n))
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

(defn- back-substitution
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

(defn solve-linear
  "Solve complex linear system A x = b and return complex vector representation."
  [A b]
  (let [Ar (mapv vec (:real A)) Ai (mapv vec (:imag A))
        b* (if (complex-vector? b) b (ensure-complex-vector b))
        br (vec (:real b*)) bi (vec (:imag b*))
        [Ar Ai br bi] (forward-elimination Ar Ai br bi)]
    (back-substitution Ar Ai br bi)))

(defn inverse
  "Inverse of complex matrix via Gauss-Jordan."
  [A]
  (let [Ar (mapv vec (:real A)) Ai (mapv vec (:imag A))
        n (count Ar) Ir (identity-matrix n) Ii (vec (repeat n (vec (repeat n 0.0))))]
    (loop [k 0 Ar Ar Ai Ai Ir Ir Ii Ii]
      (if (= k n)
        {:real Ir :imag Ii}
        (let [pivot (apply max-key #(math/hypot (get-in Ar [% k]) (get-in Ai [% k])) (range k n))
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

(defn spectral-norm
  "Compute spectral norm of complex matrix A via power iteration on A^H A with Rayleigh quotient convergence." [A]
  (let [n (count (:real A))
        x0 {:real (vec (repeat n (/ 1.0 (math/sqrt n)))) :imag (vec (repeat n 0.0))}
        tol 1e-12
        max-it 200
        Ah (conjugate-transpose A)]
    (loop [k 0 x x0 lambda-prev nil]
      (let [Ax (matrix-vector-product A x)
            AhAx (matrix-vector-product Ah Ax)
            lambda (let [num-r (reduce + (map (fn [a b c d] (+ (* a b) (* c d))) (:real x) (:real AhAx) (:imag x) (:imag AhAx)))]
                     (double num-r))
            nr (math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) (:real AhAx) (:imag AhAx))))
            x' {:real (mapv #(/ % nr) (:real AhAx)) :imag (mapv #(/ % nr) (:imag AhAx))}
            conv? (and lambda-prev (< (abs (- lambda lambda-prev)) (* tol (max 1.0 (abs lambda)))))]
        (if (or (>= k max-it) conv?)
          (math/sqrt (max 0.0 lambda))
          (recur (inc k) x' lambda))))))

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
  - eps    convergence tolerance on largest off-diagonal absolute value
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
  [A eps max-it]
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
                                      (let [aij (abs (double (get-in M [i j])))]
                                        (if (> aij bv) [i j aij] [bp bq bv])))
                                    [0 0 0.0]
                                    (for [i (range n) j (range (inc i) n)] [i j]))]
              (if (< val eps)
                {:eigenvalues (mapv #(get-in M [% %]) (range n))
                 :vectors (vec (apply mapv vector V))
                 :iterations iter}
                (let [app (get-in M [p p]) aqq (get-in M [q q]) apq (get-in M [p q])
                      tau (/ (- aqq app) (* 2.0 apq))
                      t (let [s (if (neg? tau) -1.0 1.0)] (/ s (+ (abs tau) (math/sqrt (+ 1.0 (* tau tau))))))
                      c (/ 1.0 (math/sqrt (+ 1.0 (* t t))))
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
  - eps magnitude threshold to select the reference component.

  Returns new complex vector map with adjusted :real/:imag.

  If all components are (near) zero the vector is returned unchanged."
  [v eps]
  (let [xr (:real v) xi (:imag v)
        n (count xr)
        ;; find reference index
        idx (first (for [i (range n)
                         :let [a (double (nth xr i)) b (double (nth xi i))
                               mag2 (+ (* a a) (* b b))]
                         :when (> mag2 (* eps eps))]
                     i))]
    (if (nil? idx)
      v
      (let [a (double (nth xr idx))
            b (double (nth xi idx))
            ;; Compute phase of reference component a+ib = r e^{i phi}
            phi (math/atan2 b a)
            c (math/cos phi)
            s (math/sin phi)
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

(defn outer-product
  "Compute outer product of complex vectors x and y.
  Returns a complex matrix in SoA form."
  [x y]
  (let [xr (:real x) xi (:imag x)
        yr (:real y) yi (:imag y)
        ;; (x y^H)_{ij} = (xr_i + i xi_i)(yr_j - i yi_j)
        real (vec (for [i (range (count xr))]
                    (vec (for [j (range (count yr))]
                           (+ (* (nth xr i) (nth yr j)) (* (nth xi i) (nth yi j)))))))
        imag (vec (for [i (range (count xr))]
                    (vec (for [j (range (count yr))]
                           (- (* (nth xi i) (nth yr j)) (* (nth xr i) (nth yi j)))))))]
    {:real real :imag imag}))

(defn trace
  "Compute trace of complex matrix A (SoA form).
  Returns a complex scalar."
  [A]
  (let [n (count (:real A))]
    (make-complex (reduce + (map (fn [i] (get-in (:real A) [i i])) (range n)))
                  (reduce + (map (fn [i] (get-in (:imag A) [i i])) (range n))))))

(defn norm2
  "Compute L2 norm of complex vector x (SoA form).
  Returns a non-negative real number."
  [x]
  ;; <x|x> for a valid inner product should be real non-negative; imaginary part ≈ 0.
  (let [ip (inner-product x x)
        re (double (:real ip))
        re (if (neg? re) (abs re) re)]
    (math/sqrt re)))

(defn unitary?
  "Check if complex matrix U is unitary (U^H U = I).
  Returns true if unitary, false otherwise.
  Optionally accepts a tolerance for numerical stability."
  ([U] (unitary? U default-tolerance))
  ([U eps]
   (let [[n m] (matrix-shape U)]
     (if (not= n m)
       false
       (let [Uh (if (complex-matrix? U) (conjugate-transpose U) (real-transpose U))
             P (if (complex-matrix? U) (matrix-multiply Uh U) (real-mul Uh U))
             I (identity-matrix n)]
         (if (complex-matrix? P)
           (close-matrices? (:real P) I eps)
           (close-matrices? P I eps)))))))

(defn eigen-hermitian
  "Compute eigen-decomposition of a complex Hermitian matrix A.
  Returns a map with eigenvalues and eigenvectors in SoA form.
  Optionally accepts a tolerance for numerical stability."
  ([A] (eigen-hermitian A default-tolerance))
  ([A eps]
   (let [X (:real A) Y (:imag A)
         [n m] (matrix-shape X)
         _ (when (not= n m) (throw (ex-info "eigen-hermitian requires square matrix" {:shape [n m]})))
         ;; Real embedding M = [[X -Y][Y X]]
         top (vec (for [i (range n)] (vec (concat (nth X i) (map #(- %) (nth Y i))))))
         bottom (vec (for [i (range n)] (vec (concat (nth Y i) (nth X i)))))
         M (vec (concat top bottom))
         N (* 2 n)
         tol (* eps (inc N))
         max-it (* 10 N N)
         {:keys [eigenvalues vectors]} (jacobi-symmetric M tol max-it)
         ;; Sort embedding eigenpairs ascending
         sorted (sort-by second (map-indexed vector eigenvalues))
         collapse-tol (* 20.0 eps (inc n))
         build-complex (fn [col-idx]
                         (let [w (nth vectors col-idx) ; length 2n
                               x (subvec w 0 n)
                               y (subvec w n N)
                               nrm (math/sqrt (reduce + (map (fn [a b] (+ (* a a) (* b b))) x y)))
                               nrm (if (pos? nrm) nrm 1.0)
                               x' (mapv #(/ % nrm) x)
                               y' (mapv #(/ % nrm) y)
                               v {:real x' :imag y'}]
                           (normalize-complex-phase v 1e-14)))
         [evals evects] (loop [pairs sorted acc-e [] acc-v []]
                          (if (empty? pairs)
                            [acc-e acc-v]
                            (let [[[idx λ] & more] pairs
                                  add? (or (empty? acc-e) (> (abs (- λ (last acc-e))) collapse-tol))]
                              (if add?
                                (recur more (conj acc-e λ) (conj acc-v (build-complex idx)))
                                (recur more acc-e acc-v)))))]
     {:eigenvalues (mapv double evals) :eigenvectors (vec evects)})))

(defn positive-semidefinite?
  "Check if complex matrix A is positive semidefinite.
  Returns true if A is PSD, false otherwise.
  Optionally accepts a tolerance for numerical stability."
  ([A] (positive-semidefinite? A default-tolerance))
  ([A eps]
   (if (hermitian? A eps)
     (let [{:keys [eigenvalues]} (eigen-hermitian A)]
       (every? #(>= % (- eps)) eigenvalues))
     false)))

(defn svd
  "Compute singular value decomposition of a complex matrix A.
  Returns a map with singular values and left/right singular vectors in SoA form.
  Optionally accepts a tolerance for numerical stability."
  ([A] (svd A default-tolerance))
  ([A eps]
   (let [Ar (:real A) Ai (:imag A)
         m (count Ar) n (count (first Ar))
         ;; Build AᴴA = (conj-transpose A) * A
         Ah {:real (real-transpose Ar) :imag (real-scale (real-transpose Ai) -1.0)}
         AhA (matrix-multiply Ah {:real Ar :imag Ai})
         {:keys [eigenvalues eigenvectors]} (eigen-hermitian AhA)
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
         compute-u (fn [sigma v]
                     (if (> sigma eps)
                       (let [u (matrix-vector-product {:real Ar :imag Ai} v)
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
         inner-c (fn [x y] (inner-product x y))
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
         rank (count (filter #(> % eps) singular-values))
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
                                  (if (> nrm (* 10 eps))
                                    (recur (conj basis (scale-c v1 (/ 1.0 nrm))) (inc i))
                                    (recur basis (inc i))))))))
         U-full (complete-basis U-cols m)
         V-full (complete-basis V-cols n)
         Vh {:real (real-transpose (mapv :real V-full))
             :imag (real-scale (real-transpose (mapv :imag V-full)) -1.0)}
         U-mat {:real (vec (apply map vector (map :real U-full)))
                :imag (vec (apply map vector (map :imag U-full)))}]
     {:U U-mat :S singular-values :Vt Vh :V† Vh})))

(defn lu-decomposition
  "Compute LU decomposition of a complex matrix A with partial pivoting.
  Returns a map with permutation vector P, lower triangular L and upper triangular U matrices in SoA form."
  ([A] (lu-decomposition A default-tolerance))
  ([A eps]
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
             (recur (inc k) Ar Ai Lr Li P))))))))

(defn qr-decomposition
  "Compute QR decomposition of a complex matrix A.
  Returns a map with orthogonal matrix Q and upper triangular matrix R in SoA form."
  [A]
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
          (recur (inc j) Q Rr Ri))))))

(defn eigen-general
  "Compute eigen-decomposition of a complex square matrix A.
  Returns a map with eigenvalues and eigenvectors in SoA form.
  Optionally accepts a tolerance for numerical stability."
  ([A] (eigen-general A default-tolerance))
  ([A eps]
   (let [[n m] (matrix-shape A)]
     (when (not= n m) (throw (ex-info "eigen-general requires square matrix" {:shape [n m]})))
     (let [tol (* eps (inc n))
           ;; Increase iteration cap; unshifted QR can be slow for clustered spectra
           max-it (* 800 n n)
           ;; Simple balancing (single pass): scale rows & cols by sqrt(row_norm/col_norm)
           balance-complex (fn [M]
                             (let [R (:real M) I (:imag M)
                                   row-norms (mapv (fn [i]
                                                     (math/sqrt (reduce + (for [j (range n)]
                                                                            (let [a (get-in R [i j]) b (get-in I [i j])] (+ (* a a) (* b b))))))) (range n))
                                   col-norms (mapv (fn [j]
                                                     (math/sqrt (reduce + (for [i (range n)]
                                                                            (let [a (get-in R [i j]) b (get-in I [i j])] (+ (* a a) (* b b))))))) (range n))]
                               {:real (vec (for [i (range n)]
                                             (vec (for [j (range n)]
                                                    (let [ri (max eps (row-norms i)) cj (max eps (col-norms j))
                                                          scale (/ 1.0 (math/sqrt (/ ri cj)))]
                                                      (* scale (get-in R [i j])))))))
                                :imag (vec (for [i (range n)]
                                             (vec (for [j (range n)]
                                                    (let [ri (max eps (row-norms i)) cj (max eps (col-norms j))
                                                          scale (/ 1.0 (math/sqrt (/ ri cj)))]
                                                      (* scale (get-in I [i j])))))))}))
           A0 (balance-complex A)
           offdiag-norm-complex (fn [M]
                                  (let [R (:real M) I (:imag M)]
                                    (math/sqrt (reduce + 0.0 (for [i (range n) j (range n) :when (> i j)]
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
                                 disc (math/sqrt (max 0.0 (- tr2 (* 4.0 det-mag))))
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
                   {:keys [Q R]} (qr-decomposition M-shift)
                   M-next (let [RQ (matrix-multiply R Q)]
                            (if (zero? μ) RQ
                                ;; add shift back: RQ + μ I
                                (let [Rr (:real RQ) Ri (:imag RQ)]
                                  {:real (vec (for [i (range n)]
                                                (vec (for [j (range n)]
                                                       (let [val (get-in Rr [i j])]
                                                         (if (= i j) (+ val μ) val))))))
                                   :imag Ri})))]
               (recur (inc k) M-next)))))))))

(defn cholesky-decomposition
  "Compute Cholesky decomposition of a complex Hermitian positive definite matrix A.
  Returns a map with lower triangular matrix L in SoA form."
  [A]
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
                          (when (> (abs diff-i) 1e-12) (throw (ex-info "Hermitian diag not real" {:i i :imag diff-i})))
                          (when (neg? diff-r) (throw (ex-info "Matrix not HPD" {:i i :value diff-r})))
                          (let [val (Math/sqrt diff-r)]
                            (recur (inc j) (assoc-in Lr [i i] val) Li)))
                        (let [lrjj (get-in Lr [j j])
                              fr (/ diff-r lrjj) fi (/ diff-i lrjj)
                              Lr (assoc-in Lr [i j] fr)
                              Li (assoc-in Li [i j] fi)]
                          (recur (inc j) Lr Li))))))]
          (recur (inc i) Lr Li))))))

;; Helper functions for real matrix operations
(defn- matrix-add-real [A B]
  "Add two real matrices"
  (vec (for [i (range (count A))]
         (vec (for [j (range (count (first A)))]
                (+ (get-in A [i j]) (get-in B [i j])))))))

(defn- matrix-scale-real [A s]
  "Scale a real matrix by scalar s"
  (vec (for [i (range (count A))]
         (vec (for [j (range (count (first A)))]
                (* s (get-in A [i j])))))))

(defn- matrix-multiply-real [A B]
  "Multiply two real matrices"
  (let [n (count A) m (count (first B)) p (count B)]
    (vec (for [i (range n)]
           (vec (for [j (range m)]
                  (reduce + (for [k (range p)]
                              (* (get-in A [i k]) (get-in B [k j]))))))))))

(defn- matrix-inverse-real [A]
  "Compute inverse of a real matrix using Gaussian elimination"
  (let [n (count A)
        ;; Create augmented matrix [A | I]
        aug (vec (for [i (range n)]
                   (vec (concat (nth A i) 
                               (for [j (range n)] (if (= i j) 1.0 0.0))))))]
    ;; Gaussian elimination with partial pivoting
    (loop [k 0 aug aug]
      (if (>= k n)
        ;; Extract the right half (the inverse)
        (vec (for [i (range n)]
               (vec (subvec (nth aug i) n))))
        ;; Find pivot
        (let [pivot-row (+ k (apply max-key #(abs (get-in aug [% k])) (range k n)))
              aug (if (= pivot-row k) aug
                      ;; Swap rows
                      (assoc aug k (nth aug pivot-row) pivot-row (nth aug k)))
              pivot (get-in aug [k k])]
          (if (< (abs pivot) 1e-12)
            (throw (ex-info "Matrix is singular" {}))
            ;; Eliminate column k
            (let [aug (assoc-in aug [k] (vec (map #(/ % pivot) (nth aug k))))
                  aug (loop [i 0 aug aug]
                        (if (>= i n) aug
                            (if (= i k) (recur (inc i) aug)
                                (let [factor (get-in aug [i k])
                                      new-row (vec (map - (nth aug i) 
                                                       (map #(* factor %) (nth aug k))))]
                                  (recur (inc i) (assoc aug i new-row))))))]
              (recur (inc k) aug))))))))

(defn- real-matrix-exp
  "Compute matrix exponential of a real matrix using scaling and squaring with Padé approximation.
  Input: A real matrix as vector of vectors [[...][...]]
  Output: A real matrix as vector of vectors"
  [A]
  (let [n (count A)]
    (cond
      ;; 1x1 case
      (= n 1)
      [[(math/exp (get-in A [0 0]))]]
      
      ;; Small matrices: use Taylor series
      (<= n 2)
      (let [max-norm (apply max (for [i (range n) j (range n)] (abs (get-in A [i j]))))]
        (if (< max-norm 0.1)
          ;; Taylor series: exp(A) ≈ I + A + A²/2 + A³/6 + ...
          (let [I (vec (for [i (range n)] (vec (for [j (range n)] (if (= i j) 1.0 0.0)))))
                A2 (matrix-multiply-real A A)
                A3 (matrix-multiply-real A2 A)]
            (matrix-add-real 
              (matrix-add-real 
                (matrix-add-real I A)
                (matrix-scale-real A2 0.5))
              (matrix-scale-real A3 (/ 1.0 6.0))))
          ;; Scale down, compute exp, then square back up
          (let [s (math/ceil (/ (math/log (/ max-norm 0.1)) (math/log 2)))
                A-scaled (matrix-scale-real A (/ 1.0 (math/pow 2 s)))
                exp-scaled (real-matrix-exp A-scaled)]
            ;; Square s times
            (loop [k 0 result exp-scaled]
              (if (>= k s) result
                  (recur (inc k) (matrix-multiply-real result result)))))))
      
      ;; Larger matrices: use scaling and squaring
      :else
      (let [max-norm (apply max (for [i (range n) j (range n)] (abs (get-in A [i j]))))
            s (max 0 (math/ceil (/ (math/log (/ max-norm 1.0)) (math/log 2))))
            A-scaled (if (> s 0) (matrix-scale-real A (/ 1.0 (math/pow 2 s))) A)
            ;; Padé(6,6) approximation for exp(A-scaled)
            I (vec (for [i (range n)] (vec (for [j (range n)] (if (= i j) 1.0 0.0)))))
            A2 (matrix-multiply-real A-scaled A-scaled)
            A3 (matrix-multiply-real A2 A-scaled)
            A4 (matrix-multiply-real A2 A2)
            A5 (matrix-multiply-real A3 A2)
            A6 (matrix-multiply-real A3 A3)
            ;; Numerator: I + A + A²/2 + A³/6 + A⁴/24 + A⁵/120 + A⁶/720
            num (matrix-add-real I
                  (matrix-add-real A-scaled
                    (matrix-add-real (matrix-scale-real A2 0.5)
                      (matrix-add-real (matrix-scale-real A3 (/ 1.0 6.0))
                        (matrix-add-real (matrix-scale-real A4 (/ 1.0 24.0))
                          (matrix-add-real (matrix-scale-real A5 (/ 1.0 120.0))
                            (matrix-scale-real A6 (/ 1.0 720.0))))))))
            ;; Denominator: I - A + A²/2 - A³/6 + A⁴/24 - A⁵/120 + A⁶/720
            den (matrix-add-real I
                  (matrix-add-real (matrix-scale-real A-scaled -1.0)
                    (matrix-add-real (matrix-scale-real A2 0.5)
                      (matrix-add-real (matrix-scale-real A3 (/ -1.0 6.0))
                        (matrix-add-real (matrix-scale-real A4 (/ 1.0 24.0))
                          (matrix-add-real (matrix-scale-real A5 (/ -1.0 120.0))
                            (matrix-scale-real A6 (/ 1.0 720.0))))))))
            ;; Solve den * X = num for X (i.e., X = den^(-1) * num)
            exp-scaled (try
                         (let [den-inv (matrix-inverse-real den)]
                           (matrix-multiply-real den-inv num))
                         (catch Exception _
                           ;; Fallback to Taylor series if inverse fails
                           (matrix-add-real I (matrix-add-real A-scaled (matrix-scale-real A2 0.5)))))]
        ;; Square s times to get exp(A)
        (loop [k 0 result exp-scaled]
          (if (>= k s) result
              (recur (inc k) (matrix-multiply-real result result))))))))

(defn matrix-exp
  "Compute matrix exponential of a complex matrix A.
  Returns a complex matrix in SoA form."
  [A]
  (let [X (:real A) Y (:imag A)
        n (count X)]
    (cond
      ;; 1x1 complex scalar
      (and (= 1 n) (= 1 (count (first X))))
      (let [a (double (get-in X [0 0])) b (double (get-in Y [0 0])) ea (math/exp a)]
        {:real [[(* ea (math/cos b))]]
         :imag [[(* ea (math/sin b))]]})
      ;; Diagonal complex matrix: exp acts element-wise
      (every? true? (for [i (range n) j (range n) :when (not= i j)]
                      (and (zero? (double (get-in X [i j]))) (zero? (double (get-in Y [i j]))))))
      (let [R (vec (for [i (range n)]
                     (vec (for [j (range n)]
                            (if (= i j)
                              (let [a (double (get-in X [i i])) b (double (get-in Y [i i])) ea (math/exp a)]
                                (* ea (math/cos b)))
                              0.0)))))
            I (vec (for [i (range n)]
                     (vec (for [j (range n)]
                            (if (= i j)
                              (let [a (double (get-in X [i i])) b (double (get-in Y [i i])) ea (math/exp a)]
                                (* ea (math/sin b)))
                              0.0)))))]
        {:real R :imag I})
      :else
      ;; General complex: embed A = X + iY into real block [[X -Y][Y X]] and exponentiate
      (let [Z (vec (for [i (range n)] (vec (concat (nth X i) (map #(- (double %)) (nth Y i))))))
            Z2 (vec (for [i (range n)] (vec (concat (nth Y i) (nth X i)))))
            B (into [] (concat Z Z2))
            ;; Use real matrix exponential for the embedded real matrix
            E (real-matrix-exp B)
            Er (vec (for [i (range n)] (subvec (E i) 0 n)))
            Ei (vec (for [i (range n)] (subvec (E (+ i n)) 0 n)))]
        {:real Er :imag Ei}))))

(defn matrix-log
  "Compute matrix logarithm of a complex matrix A.
  Returns a complex matrix in SoA form."
  [A]
      ;; Matrix logarithm implementation.
    ;; supports Hermitian positive definite complex matrix.
    (let [[n _] (matrix-shape A)]
      (when (zero? n) {:real [] :imag []})
      (when (not (hermitian? A 1e-10)) (throw (ex-info "matrix-log implemented only for Hermitian positive definite complex matrices" {:matrix A})))
      ;; Use eigen-decomposition A = V Λ V^H, then log(A) = V log(Λ) V^H
      (let [{:keys [eigenvalues eigenvectors]} (eigen-hermitian A)]
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
          {:real real :imag imag}))))

(defn matrix-sqrt
  "Compute matrix square root of a complex matrix A.
  Returns a complex matrix in SoA form."
  [A]
  ;; Matrix square root.
  ;; Hermitian positive semidefinite complex matrix via complex Denman–Beavers.
  (let [[n n2] (matrix-shape A)]
    (when (not= n n2) (throw (ex-info "matrix-sqrt requires square" {:shape [n n2]})))
    (when (not (hermitian? A 1e-10)) (throw (ex-info "Matrix square root requires positive semidefinite matrix" {:matrix A :reason :not-hermitian})))
    (when (not (positive-semidefinite? A)) (throw (ex-info "Matrix square root requires positive semidefinite matrix" {:matrix A :reason :not-psd})))
    (let [I {:real (identity-matrix n)
             :imag (vec (repeat n (vec (repeat n 0.0))))}
          tol 1e-10
          max-it 60]
      (loop [k 0 Y A Z I]
        (let [Zinv (inverse Z)
              Yinv (inverse Y)
              Ynext (matrix-scale (matrix-add Y Zinv) 0.5)
              Znext (matrix-scale (matrix-add Z Yinv) 0.5)
              diff (let [YY (matrix-multiply Ynext Ynext)
                         D (matrix-subtract YY A)
                         nr (Math/sqrt (reduce + (for [i (range n) j (range n)]
                                                   (let [dr (get-in (:real D) [i j])
                                                         di (get-in (:imag D) [i j])]
                                                     (+ (* dr dr) (* di di))))))]
                     nr)]
          (if (or (< diff tol) (>= k max-it))
            Ynext
            (recur (inc k) Ynext Znext)))))))

(defn condition-number
  "Compute condition number of a complex matrix A.
  Returns a positive real number or Double/POSITIVE_INFINITY if A is singular."
  [A]
  (let [normA (spectral-norm A)
        invA (try (inverse A)
                  (catch Exception _ nil))]
    (if invA
      (let [normInv (spectral-norm invA)]
        (* normA normInv))
      Double/POSITIVE_INFINITY)))

;;;
;;; Quantum State Operation Functions (TODO: Move to separate namespace)
;;;
(defn state-normalize
  "Normalize a quantum state vector psi.
  Returns a new state vector with unit norm."
  [state]
  (let [nrm (norm2 state)
        s (if (pos? nrm) (/ 1.0 nrm) 1.0)]
    {:real (mapv #(* s %) (:real state))
     :imag (mapv #(* s %) (:imag state))}))

(defn projector-from-state
  "Compute the projector |psi><psi| from a quantum state vector psi.
  Returns a density matrix in SoA form."
  [psi]
  (outer-product psi psi))

(defn density-matrix
  "Compute the density matrix from a pure state vector psi.
  Returns a density matrix in SoA form."
  [psi]
  (projector-from-state psi))

(defn trace-one?
  "Check if a density matrix rho has trace one.
  Optionally accepts a tolerance for numerical stability."
  ([rho] (trace-one? rho default-tolerance))
  ([rho eps]
   (let [tr (trace rho)
         eps (double (or eps default-tolerance))]
     (and (< (abs (- (:real tr) 1.0)) eps)
          (< (abs (:imag tr)) eps)))))

