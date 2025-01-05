(ns clj-arsenal.burp
  #?(:cljs (:require-macros clj-arsenal.burp))
  (:require
   #?(:clj [clj-arsenal.burp.macro-impl :as macro-impl])
   [clj-arsenal.burp.impl :refer [->BurpElement ->BurpElementKey] :as impl]
   [clj-arsenal.check :refer [check expect]]
   [clj-arsenal.basis.once]))

#?(:clj
   (defmacro burp "
Expands to a list of burp elements, converted from the hiccup `forms`.
Constants found within the forms will be extracted and reused.
" [& forms]
     `(impl/-flatten-body (list ~@(map macro-impl/convert-form forms)))))
#?(:clj (defmacro $ [& forms] (macro-impl/convert-form (into (with-meta [] (meta &form)) forms))))

(defn element? "
Return true if `x` is burp element.  Burp elements are records with
#{:key :props :body}.
" [x]
  (impl/element? x))

(defn element-key? "
Returns true if `x` is a burp key.  Burp keys are records with
#{:operator :custom-key}, where `:operator` is the thing at
the head of a hiccup form, and `:custom-key` is taken from
the `:key` metadata on the hiccup form, or `nil`.
" [x]
  (impl/element-key? x))


(defn element-key "
Create an element key record.
" [operator custom-key]
  (->BurpElementKey operator custom-key))
