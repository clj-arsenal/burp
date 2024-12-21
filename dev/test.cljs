(ns test
  (:require
   [clj-arsenal.burp :refer [burp $]]
   [clj-arsenal.burp.impl :refer [->BurpElement ->BurpElementKey]]
   [clj-arsenal.check :refer [check expect]]))

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

(check ::burp
 (let [the-key (gensym)]
   (expect =
     (burp
       ^{:key the-key}
       [:foo#fooz.bar.baz {:blah 1 :bleh 2} :fee :fi :fo :fum])
     (->BurpElement
       (->BurpElementKey :foo the-key)
       {:clj-arsenal.burp/id "fooz"
        :clj-arsenal.burp/classes #{"bar" "baz"}
        :blah 1
        :bleh 2}
       (list :fee :fi :fo :fum)))))

(defn run
  []
  )
