(ns ^:no-doc clj-arsenal.burp.macro-impl
  (:require
   [clojure.string :as str]
   [clj-arsenal.burp.impl :refer [->BurpElementKey ->BurpElement] :as impl]
   [clj-arsenal.check :refer [check when-check expect]]
   [clj-arsenal.basis.once :refer [once constant?]]
   [clojure.walk :as walk]))

(defn- ^:macro-support argument-error
  [msg]
  #?(:cljs (js/Error. msg)
     :cljd/host (IllegalArgumentException. msg)
     :cljd (ArgumentError msg)
     :clj (IllegalArgumentException. msg)))

(defn- ^:macro-support normalize-element-node
  [[operator & others :as form]]
  (if (or (symbol? operator) (keyword? operator)
        (and (vector operator)
          (every? #(or (keyword? %) (symbol? %)) operator)))
    (if (map? (first others))
      (let [props (first others)]
        (when-not (every? keyword? (keys props))
          (throw (argument-error "burp prop keys must be keyword literals")))
        [operator props (rest others)])
      [operator {} others])
    (throw (argument-error "burp operator must be a keyword, a symbol, or a vector of the same"))))

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

(defn- ^:macro-support burpify
  [form]
  (if-not (vector? form)
    form
    (let [[operator props body] (normalize-element-node form)]
      (cond
        (keyword? operator)
        (let [[tag tag-props] (parse-tag-props operator)]
          (->BurpElement
            (->BurpElementKey tag (:key (meta form)))
            (merge props tag-props)
            (map burpify body)))

        (vector? operator)
        (let [parsed-tag-components (mapv #(if (keyword? %) (parse-tag-props %) [% {}]) operator)]
          (reduce
            (fn [inner-burp-element [component-tag component-props]]
              (->BurpElement
                (->BurpElementKey component-tag nil)
                component-props
                [inner-burp-element]))
            (let [[innermost-tag innermost-props] (peek parsed-tag-components)]
              (->BurpElement
                (->BurpElementKey innermost-tag (:key (meta form)))
                (merge props innermost-props)
                (map burpify body)))
            (reverse (subvec parsed-tag-components 0 (dec (count parsed-tag-components))))))))))

(check ::burpify
  (expect =
    (burpify ^{:key :my-key} [:foo#fooz.bar.baz {:blah 1 :bleh 2} 1 2 3])
    (->BurpElement
      (->BurpElementKey :foo :my-key)
      {:clj-arsenal.burp/id "fooz"
       :clj-arsenal.burp/classes #{"bar" "baz"}
       :blah 1
       :bleh 2}
      (list 1 2 3))))

(defn- ^:macro-support onceify*
  [form]
  (if-not (or (map? form) (set? form) (vector? form))
    form
    (if (and (constant? form) (not (map-entry? form)))
      `(once ~form)
      (walk/walk onceify* identity form))))

(defn- ^:macro-support onceify
  [form]
  (if-not (impl/element? form)
    form
    (onceify* form)))

(defn ^:macro-support convert-form
  [form]
  (->> form burpify onceify
    (walk/postwalk
      (fn [x]
        (cond
          (impl/element? x)
          `(->BurpElement ~(:key x) ~(:props x) (impl/-flatten-body (list ~@(:body x))))

          (impl/element-key? x)
          `(->BurpElementKey ~(:operator x) ~(:custom-key x))

          :else
          x)))))

(defn ^:macro-support convert-forms
  [forms]
  (if (empty? (rest forms))
    (convert-form (first forms))
    (cons 'clojure.core/list (map convert-form forms))))
