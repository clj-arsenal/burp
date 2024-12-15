(ns test
  (:require
   [clj-arsenal.burp :refer [burp $]]))

(comment
  :cljs/exit
  (shadow.cljs.devtools.api/repl :dev))

(burp
  [:foo {:bar 1 :baz 2} (identity 1)]
  [:bar {:bleh 2}])

(identity
^{:key :blah}
 ($ :foo {:bar "foo" :baz "bleh"}
  (range 0 10)))

(defn run
  []
  )
