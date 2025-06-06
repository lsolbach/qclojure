(defproject org.soulspace/qclojure "0.1.0-SNAPSHOT"
  :description "A library to implement and simulate quantum and hybrid algorithms in idiomatic Clojure"
  :license {:name "Eclipse Public License 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/data.json "2.4.0"]
                 [org.clojure/spec.alpha "0.5.238"]
                 [org.clojure/test.check "1.1.1"]
                 [generateme/fastmath "3.0.0-alpha3"]
                 [hiccup "2.0.0-RC5"]]

  :profiles {:clay {:dependencies [[org.scicloj/clay "2-beta45"]]
                    :source-paths ["src" "notebook"]}}
  
  :scm {:name "git" :url "https://github.com/lsolbach/QClojure"}
  :deploy-repositories [["clojars" {:sign-releases false :url "https://clojars.org/repo"}]]
  ;
  )
