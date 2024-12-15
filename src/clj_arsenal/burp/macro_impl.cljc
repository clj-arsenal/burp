(ns ^:no-doc clj-arsenal.burp.macro-impl
  (:require
   [clojure.string :as str]
   [clj-arsenal.burp.impl :refer [->BurpElementKey ->BurpElement] :as impl]
   [clj-arsenal.check :refer [check when-check expect]]
   [clojure.walk :as walk]))

(defonce !once-key-cache (atom {}))

(defmacro once
  [x]
  (let [k (or (get @!once-key-cache x)
            (let [k' (str (gensym "burp-const"))]
              (swap! !once-key-cache assoc x k')
              k'))]
    `(or (impl/once-cache-get ~k)
       (impl/once-cache-put! ~k ~x))))

(defn- argument-error
  [msg]
  #?(:cljs (js/Error. msg)
     :cljd (ArgumentError msg)
     :clj (IllegalArgumentException. msg)))

(defn- normalize-element-node
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

(defn- parse-tag-props
  [tag]
  (if-some [[_ tag-name id classes] (re-matches #"^([^#.]+)([#][^.]+)?([.].+)?$" (name tag))]
    [(keyword (namespace tag) tag-name)
     {:id (some-> id (subs 1))
      :class (some-> classes
               (as-> $
                 (str/split $ #"[.]")
                 (remove str/blank? $)
                 (str/join " " $)))}]
    (throw (argument-error "invalid keyword for burp operator"))))

(defn- merge-inline-tag-props-with-explicit-prop-map
  [inline-tag-props explicit-prop-map]
  (cond-> explicit-prop-map
    (not (str/blank? (:class inline-tag-props)))
    (assoc
      :a/class
      (if (str/blank? (:a/class explicit-prop-map))
        (:class inline-tag-props)
        (str (:class inline-tag-props) " " (:a/class explicit-prop-map)))
      
      :className
      (if (str/blank? (:className explicit-prop-map))
        (:class inline-tag-props)
        (str (:class inline-tag-props) " " (:className explicit-prop-map))))

    (and (not (str/blank? (:id inline-tag-props))) (not (:id explicit-prop-map)))
    (assoc :id (:id inline-tag-props))))

(defn- burpify
  [form]
  (if-not (vector? form)
    form
    (let [[operator props body] (normalize-element-node form)]
      (cond
        (keyword? operator)
        (let [[tag tag-props] (parse-tag-props operator)]
          (->BurpElement
            (->BurpElementKey tag (:key (meta form)))
            (merge-inline-tag-props-with-explicit-prop-map tag-props props)
            (mapv burpify body)))

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
                (merge-inline-tag-props-with-explicit-prop-map innermost-props props)
                (mapv burpify body)))
            (reverse (subvec parsed-tag-components 0 (dec (count parsed-tag-components))))))))))

(check ::burpify
  (expect =
    (burpify ^{:key :my-key} [:foo#fooz.bar.baz {:blah 1 :bleh 2} 1 2 3])
    (->BurpElement
      (->BurpElementKey :foo :my-key)
      {:className "bar baz" :a/class "bar baz" :id "fooz" :blah 1 :bleh 2}
      [1 2 3])))

(defn- constant?
  [form]
  (or
    (keyword? form)
    (string? form)
    (number? form)
    (nil? form)
    (and (seq? form) (= `once (first form)))
    (and (coll? form) (every? constant? form))
    (and (tagged-literal? form) (constant? (:form form)))))

(defn- onceify*
  [form]
  (if-not (or (map? form) (set? form) (vector? form))
    form
    (if (and (constant? form) (not (map-entry? form)))
      `(once ~form)
      (walk/walk onceify* identity form))))

(defn- onceify
  [form]
  (if-not (impl/element? form)
    form
    (onceify* form)))

(defn convert-form
  [form]
  (->> form burpify onceify
    (walk/postwalk
      (fn [x]
        (cond
          (impl/element? x)
          `(->BurpElement ~(:key x) ~(:props x) ~(:body x))

          (impl/element-key? x)
          `(->BurpElementKey ~(:tag x) ~(:custom-key x))

          :else
          x)))))

(defn convert-forms
  [forms]
  (if (nil? (rest forms))
    (convert-form (first forms))
    (cons 'clojure.core/list (map convert-form forms))))
