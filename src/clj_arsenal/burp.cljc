(ns ^:no-doc clj-arsenal.burp
  #?(:cljs (:require-macros clj-arsenal.burp))
  (:require
   #?(:clj [clj-arsenal.burp.macro-impl :as macro-impl])
   [clj-arsenal.check :refer [check expect] :as check]
   [clj-arsenal.basis.once]))

(defrecord BurpElementKey [operator custom-key])
(defrecord BurpElement [key props body])

(defn element? "
Return true if `x` is burp element.  Burp elements are records with
#{:key :props :body}.
" [x]
  (instance? BurpElement x))

(defn element-key? "
Returns true if `x` is a burp key.  Burp keys are records with
#{:operator :custom-key}, where `:operator` is the thing at
the head of a hiccup form, and `:custom-key` is taken from
the `:key` metadata on the hiccup form, or `nil`.
" [x]
  (instance? BurpElementKey x))


#?(:clj
   (defmacro $
     [operator & forms]
     (macro-impl/expand-burp-element operator forms)))

(defn element-key "
Create an element key record.
" [operator custom-key]
  (->BurpElementKey operator custom-key))

(defn ^:no-doc flatten-body
  [body]
  (mapcat
    (fn [x]
      (cond
        (seq? x) (flatten-body x)
        (nil? x) nil
        :else [x]))
    body)
  (mapcat
    (fn [x]
      (cond
        (seq? x) (flatten-body x)
        (nil? x) nil
        :else [x]))
    body))

(check ::elements
  (let
    [the-key (gensym)]
    (expect =
      (macroexpand
      '(clj-arsenal.burp/$ :foo#fooz.bar.baz :> the-key
        {:blah 1 :bleh 2}
        :fee :fi :fo :fum))
      (->BurpElement
        (->BurpElementKey :foo the-key)
        {:clj-arsenal.burp/id "fooz"
         :clj-arsenal.burp/classes #{"bar" "baz"}
         :blah 1
         :bleh 2}
        [:fee :fi :fo :fum]))
    
    (expect =
      (clj-arsenal.burp/$ :foo#fooz.bar.baz
        {:blah 1 :bleh 2}
        :fee :fi :fo :fum)
      (->BurpElement
        (->BurpElementKey :foo nil)
        {:clj-arsenal.burp/id "fooz"
         :clj-arsenal.burp/classes #{"bar" "baz"}
         :blah 1
         :bleh 2}
        [:fee :fi :fo :fum]))
    
    (expect =
      (clj-arsenal.burp/$ :foo#fooz.bar.baz
        :fee :fi :fo :fum)
      (->BurpElement
        (->BurpElementKey :foo nil)
        {:clj-arsenal.burp/id "fooz"
         :clj-arsenal.burp/classes #{"bar" "baz"}}
        [:fee :fi :fo :fum]))))

(check ::onceify-ok
  (let
    [count (atom 0)
     f #(clj-arsenal.burp/$ :foo#fooz.bar.baz (do (swap! count inc) nil))]
    (f) (f)
    (expect = @count 2)))
