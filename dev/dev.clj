(ns dev
  (:require [clojure.repl :as repl]
            [scicloj.clay.v2.api :as clay]))

(comment ; code exploration
  
  ; Aliases of the required namespaces
  (ns-aliases 'org.soulspace.qclojure.domain.state)

  ; Public vars in the namespace
  (ns-publics 'org.soulspace.qclojure.domain.state)

  ; Referred vars in the namespace
  (ns-refers 'org.soulspace.qclojure.domain.state)
  
  ; Vars in the namespace (including private vars)
  (ns-interns 'org.soulspace.qclojure.domain.state)

  ; Imported Java classes in the namespace
  (ns-imports 'org.soulspace.qclojure.domain.state)

  ; All the vars in the namespace
  (ns-map 'org.soulspace.qclojure.domain.state)

  (repl/dir org.soulspace.qclojure.domain.state)

  ; Documentation of a var
  (repl/doc org.soulspace.qclojure.domain.state/bits-to-index)
  (repl/source org.soulspace.qclojure.domain.state/bits-to-index)

  (require 'org.soulspace.qclojure.adapter.visualization)
  (require 'org.soulspace.qclojure.adapter.visualization.common)
  (require 'org.soulspace.qclojure.adapter.visualization.ascii)
  (require 'org.soulspace.qclojure.adapter.visualization.svg)
  (methods org.soulspace.qclojure.adapter.visualization/visualize-circuit)

  ;
  )

(comment ; Clay 
  (clay/make! {:base-source-path "notebook"
               :source-path ["qclojure.clj"
                             "quantum_computing.clj"
                             "tutorial.clj"]
               :format [:html]
               :title "QClojure Docs"
               :hide-ui-header true
               :hide-info-line true
               ;:live-reload true
               :browse true})
  ;
  )
