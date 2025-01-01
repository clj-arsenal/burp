(ns clj-arsenal.burp
  #?(:cljs (:require-macros clj-arsenal.burp))
  (:require
   #?(:clj [clj-arsenal.burp.macro-impl :as macro-impl])
   [clj-arsenal.burp.impl :refer [->BurpElement ->BurpElementKey] :as impl]
   [clj-arsenal.check :refer [check expect]]
   [clj-arsenal.basis.once]))

#?(:clj (defmacro burp [& forms] `(impl/-flatten-body (list ~@(map macro-impl/convert-form forms)))))
#?(:clj (defmacro $ [& forms] (macro-impl/convert-form (into (with-meta [] (meta &form)) forms))))

(defn element?
  [x]
  (impl/element? x))

(defn element-key?
  [x]
  (impl/element-key? x))


(defn element-key
  [operator custom-key]
  (->BurpElementKey operator custom-key))
