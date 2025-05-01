(ns ^:no-doc clj-arsenal.burp.macro-impl
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
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
     (cond-> {}
       (some? id)
       (assoc :clj-arsenal.burp/id (subs id 1))
       
       (some? classes)
       (assoc :clj-arsenal.burp/classes
        (as-> classes $
          (str/split $ #"[.]")
          (remove str/blank? $)
          (set $))))]
    (throw (argument-error "invalid keyword for burp operator"))))

(defn- ^:macro-support expand-burp-element-inner
  [burp-element-form]
  (let
    [[_ operator & contents] burp-element-form
     _ (validate-operator operator)
     [k props body] (separate-parts contents)]
    (cond
      (keyword? operator)
      (let
        [[tag tag-props] (parse-tag-props operator)]
        `(with-meta
           (burp/->BurpElement
             (burp/->BurpElementKey ~tag ~k)
             ~(merge props tag-props)
             (burp/flatten-body ~(vec body)))
           ~(meta burp-element-form)))

      (vector? operator)
      (let
        [parsed-tag-components
         (mapv #(if (keyword? %) (parse-tag-props %) [% {}]) operator)]
        (reduce
          (fn [inner-burp-element [component-tag component-props]]
            `(with-meta
               (burp/->BurpElement
                 (burp/->BurpElementKey ~component-tag ~k)
                 ~component-props
                 [~inner-burp-element])
               ~(meta burp-element-form)))
          (let
            [[innermost-tag innermost-props]
             (peek parsed-tag-components)]
            `(with-meta
               (burp/->BurpElement
                 (burp/->BurpElementKey ~innermost-tag ~k)
                 ~(merge props innermost-props)
                 (burp/flatten-body ~(vec body)))
               ~(meta burp-element-form)))
          (reverse
            (subvec
              parsed-tag-components
              0
              (dec (count parsed-tag-components)))))))))

(defn ^:macro-support expand-burp-element
  [burp-element-form]
  (let [macro-sym (first burp-element-form)]
    `(onceify
       ~(walk/postwalk
          (fn [x]
            (if (and (seq? x) (= (first x) macro-sym))
              (expand-burp-element-inner x)
              x))
          burp-element-form))))

