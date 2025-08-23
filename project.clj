(defproject org.soulspace/qclojure "0.11.0-SNAPSHOT"
  :description "A library to implement and simulate quantum and hybrid algorithms in idiomatic Clojure"
  :license {:name "Eclipse Public License 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [org.clojure/data.json "2.5.1"]
                 [org.clojure/spec.alpha "0.5.238"]
                 [org.clojure/test.check "1.1.1"]
                 
                 [org.uncomplicate/neanderthal-base "0.55.2"]
                 ;; Optional, for CPU computing with OpenBLAS
                 [org.uncomplicate/neanderthal-openblas "0.55.0"]
                 ;; Optional, for GPU computing with OpenCL
                 [org.uncomplicate/neanderthal-opencl "0.55.0"] 
                 
                 ; [org.bytedeco/mkl-platform "2024.0-1.5.10"]
                 ; [uncomplicate/clojurecl "0.16.1"]
                 [generateme/fastmath "3.0.0-alpha3"]
                 [hiccup/hiccup "2.0.0"]]

  ;; We need this for the CUDA binaries, which are not available in the Maven Central due to its huge size (3GB, vs 1GB limit)!
  :repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots"]]

  ;; We need direct linking for properly resolving types in heavy macros and avoiding reflection warnings!
;  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"
;                       "--enable-native-access=ALL-UNNAMED"]
  :jvm-opts ^:replace ["-Dclojure.compiler.direct-linking=true"
                       "--enable-native-access=ALL-UNNAMED"]

  :profiles {:dev [:user {}]
             :clay {:dependencies [[org.scicloj/clay "2-beta52"]]
                    :source-paths ["src" "notebook"]}
             :default [:default/all ~(leiningen.core.utils/get-os)]
             :default/all {:dependencies [[org.bytedeco/openblas "0.3.30-1.5.12"]]}
             :linux {:dependencies [[org.bytedeco/openblas "0.3.30-1.5.12" :classifier "linux-x86_64"]
                                    [org.uncomplicate/neanderthal-mkl "0.55.0"]
                                    [org.bytedeco/mkl "2025.2-1.5.12" :classifier "linux-x86_64-redist"]
                                    [org.uncomplicate/neanderthal-cuda "0.55.0"]
                                    [org.bytedeco/cuda "12.9-9.10-1.5.12-20250612.143830-1" :classifier "linux-x86_64-redist"]]}
             :windows {:dependencies [[org.bytedeco/openblas "0.3.30-1.5.12" :classifier windows-x86_64]
                                      [org.uncomplicate/neanderthal-mkl "0.55.0"]
                                      [org.bytedeco/mkl "2025.2-1.5.12" :classifier "windows-x86_64-redist"]
                                      [org.uncomplicate/neanderthal-cuda "0.55.0"]
                                      [org.bytedeco/cuda "12.9-9.10-1.5.12-20250612.145546-3" :classifier "windows-x86_64-redist"]]}
             :macosx {:dependencies [[org.uncomplicate/neanderthal-accelerate "0.55.0"]
                                     [org.bytedeco/openblas "0.3.30-1.5.12" :classifier "macosx-arm64"]]}}

  :scm {:name "git" :url "https://github.com/lsolbach/qclojure"}
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]]
  ;
  )
