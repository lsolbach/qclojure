(defproject org.soulspace/qclojure "0.11.0-SNAPSHOT"
  :description "A library to implement and simulate quantum and hybrid algorithms in idiomatic Clojure"
  :license {:name "Eclipse Public License 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [org.clojure/data.json "2.5.1"]
                 [org.clojure/spec.alpha "0.5.238"]
                 [org.clojure/test.check "1.1.1"]
                 ;[uncomplicate/neanderthal "0.55.0"] ; AOT compiled one stop with fast startup
                 [org.uncomplicate/neanderthal-base "0.55.1"] ; Base library for Neanderthal, non AOT compiled
                 [org.uncomplicate/neanderthal-opencl "0.55.0"] ; OpenCL backend for Neanderthal, non AOT compiled
                 [org.uncomplicate/neanderthal-mkl "0.55.0"] ; MKL backend for Neanderthal, non AOT compiled
                 ;[org.uncomplicate/neanderthal-cuda "0.55.0"] ; CUDA backend for Neanderthal, non AOT compiled
                 ;[org.uncomplicate/neanderthal-accelerate "0.55.0"] ; Apple Accelerate backend for Neanderthal, non AOT compiled
                 [generateme/fastmath "3.0.0-alpha3"]
                 [hiccup/hiccup "2.0.0"]]

  ; TODO: Provide native library path per platform
  :jvm-opts ["-Djava.library.path=/usr/lib/x86_64-linux-gnu"]
  :profiles {:clay {:dependencies [[org.scicloj/clay "2-beta45"]]
                    :source-paths ["src" "notebook"]}}

  :scm {:name "git" :url "https://github.com/lsolbach/qclojure"}
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]]
  ;
  )
