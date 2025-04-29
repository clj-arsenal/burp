(ns ^:no-doc clj-arsenal.burp.macro-impl
  (:require
   [clojure.string :as str]
   [clj-arsenal.basis.once :refer [onceify]]
   [clj-arsenal.burp :as-alias burp]))

(defn- ^:macro-support argument-error
  [msg]
  #?(:cljs (js/Error. msg)
     :cljd/host (IllegalArgumentException. msg)
     :cljd (ArgumentError msg)
     :clj (IllegalArgumentException. msg)))

(defn- ^:macro-support validate-operator
  [operator]
  (when-not
    (or (symbol? operator) (keyword? operator)
      (and (vector operator)
        (every? #(or (keyword? %) (symbol? %)) operator)))
    (throw (argument-error "burp operator must be a keyword, a symbol, or a vector of the same"))))

(defn- ^:macro-support separate-parts
  [[form1 & other-forms :as all-forms]]
  (cond
    (= :> form1)
    (let
      [[form2 & other-forms] other-forms]
      (if (map? (first other-forms))
        [form2 (first other-forms) (rest other-forms)]
        [form2 {} other-forms]))
    
    (map? form1)
    [nil form1 other-forms]
    
    :else
    [nil {} all-forms]))

(defn- ^:macro-support parse-tag-props
  [tag]
  (if-some [[_ tag-name id classes] (re-matches #"^([^#.]+)([#][^.]+)?([.].+)?$" (name tag))]
    [(keyword (namespace tag) tag-name)
     {:clj-arsenal.burp/id
      (some-> id (subs 1))
      :clj-arsenal.burp/classes
      (some-> classes
        (as-> $
          (str/split $ #"[.]")
          (remove str/blank? $)
          (set $)))}]
    (throw (argument-error "invalid keyword for burp operator"))))

(defn ^:macro-support expand-burp-element
  [operator forms]
  (validate-operator operator)
  (let
    [[k props body] (separate-parts forms)]
    (cond
      (keyword? operator)
      (let
        [[tag tag-props] (parse-tag-props operator)]
          `(burp/->BurpElement
             (burp/->BurpElementKey ~tag ~k)
             ~(merge props tag-props)
             (burp/flatten-body ~(vec body))))

      (vector? operator)
      (let
        [parsed-tag-components
         (mapv #(if (keyword? %) (parse-tag-props %) [% {}]) operator)]
        `(onceify
           ~(reduce
              (fn [inner-burp-element [component-tag component-props]]
                `(burp/->BurpElement
                   (burp/->BurpElementKey ~component-tag ~k)
                   ~component-props
                   [~inner-burp-element]))
              (let [[innermost-tag innermost-props] (peek parsed-tag-components)]
                `(burp/->BurpElement
                   (burp/->BurpElementKey ~innermost-tag ~k)
                   ~(merge props innermost-props)
                   (burp/flatten-body ~(vec body))))
              (reverse
                (subvec
                  parsed-tag-components
                  0
                  (dec (count parsed-tag-components))))))))))

