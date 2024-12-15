(ns clj-arsenal.burp
  #?(:cljs (:require-macros clj-arsenal.burp))
  (:require
   [clj-arsenal.burp.macro-impl :as macro-impl]
   [clj-arsenal.burp.impl]))

#?(:clj (defmacro burp [& forms] (macro-impl/convert-forms forms)))
#?(:clj (defmacro $ [& forms] (macro-impl/convert-form (into (with-meta [] (meta &form)) forms))))
