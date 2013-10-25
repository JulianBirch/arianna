(ns arianna.api
  (:refer-clojure :exclude [and comp cond or when ->>])
  (:import [clojure.lang IPersistentVector ISeq Keyword])
  (:require [arianna.runtime :as r]
            [spyscope.core]
            [poppea :refer [document-partial-% partial-invoke-%
                            defn-curried]]))

(defn valid-projection? [proj]
  (clojure.core/or (keyword? proj)
                   (vector? proj)))
;;; Validator-creating macros

(defmacro ^:validator is
  "Returns a validator which will call (predicate ~@args input).
   Can use clojure's % syntax like an anonymous function, so
   (v/is < % 10) will call (< input 10).

   predicate must be a symbol.

   Example usages
   (v/is string?)
   (v/is < % 10)"
  [predicate & args]
  {:pre [(symbol? predicate)]}
  `(assoc (document-partial-% ~predicate ~@args)
     :-method `r/is))

(defmacro ^:validator is-optional
  "Returns a validator, like is, which will call
   (predicate ~@args input). The validation result is always true,
   but if the test is true the composite predicate will immediately
   short-circuit.

   Example usage:
   (v/is-optional v/absent? :nil :blank)

   Which prevents evaluation of subsequent validators if the value
   is nil or blank.

   predicate must be a symbol."
  [predicate & args]
  {:pre [(symbol? predicate)]}
  `(assoc (document-partial-% ~predicate ~@args)
     :-method `r/is-optional))

(defmacro ^:validator is-not
  "Returns a validator, like is,  which will call
  (not (predicate ~@args input)). predicate must be a symbol."
  [predicate & args]
  {:pre [(symbol? predicate)]}
  `(assoc (document-partial-% ~predicate ~@args)
     :-method `r/is-not))

(defmacro ^:validator as
  "Returns a validator which will call a projection function.
   Will fail if the returned value is equal to :-fail.  By default,
   this means it will fail if the returned value is nil."
  [projection & args]
  {:pre [(symbol? projection)]}
  `(assoc (document-partial-% ~projection ~@args)
     :-method `r/as))

(defn strip-vector [p]
  (if (clojure.core/and (vector? p) (= 1 (count p)))
    (first p)
    p))

(defn ^:validator as-key
  "Returns a validator which will look into a map.
   Takes a keyword or a vector.  as-key never fails and
   if the map doesn't have the key specified the result
   will be the special value :arianna/missing.  You can test
   for this with v/required, v/optional or
   (v/is v/absent? :missing).  (The first two also test for
   nil and blank strings.)"
  [projection]
  {:pre [(valid-projection? projection)]}
  {:-method `r/as-key
   :projection (strip-vector projection)
   :default :arianna/missing})

(defn ^:validator has
  "Returns a validator which will call (proj ~@args input).
  proj must be a symbol.  has will fail if the map doesn't have the
  key specified."
  [projection]
  {:pre [(valid-projection? projection)]}
  {:-method `r/has
   :projection (strip-vector projection)})

;;; interpreting things as is or as

(defn- enhancable? [s]
  (if (symbol? s)
    (if-let [r (resolve s)]
      (if (-> r meta :validator not)
        (fn? @r)))
    (fn? s)))

(defn- ends-with? [^String s ^String x]
  (.endsWith s x))

(def predicate-operators #{"<" "<=" "=" "==" ">=" ">"})

(defn- predicate-symbol? [f]
  (let [n (name f)]
    (clojure.core/or
     (predicate-operators n)
     (ends-with? n "?"))))

(defmacro interpret-internal [v as-key as]
  (clojure.core/or
   (if (valid-projection? v)
     `(~as-key ~v)
     (let [v (if (seq? v) v (list v))
           f (first v)]
       (if (enhancable? f)
         (if (predicate-symbol? f)
           `(is ~@v)
           `(~as ~@v)))))
   `(let [v# ~v]
      (if (valid-projection? v#)
        (~as-key v#)
        v#))))

(defmacro interpret-is [v]
  `(interpret-internal ~v has is))

(defmacro interpret-as [v]
  `(interpret-internal ~v as-key as))

;;; composites

(defn single
  ([[x & xs] default] (if (empty? xs) x default))
  ([list] (single list nil)))

(defn eat [pred vs]
  (let [[a n] (split-with pred (rest vs))]
    [(cons (first vs) a) n]))

(defn-curried partition-runs-f [pred [vs a] v]
  (if (pred v)
    [(conj vs a) [v]]
    [vs (conj a v)]))

(defn partition-runs [pred [x & xs]]
  (apply conj (reduce (partition-runs-f pred) [[] [x]] xs)))

(defn make-validator [vs]
  (clojure.core/->> vs
                    (map #(if (string? %) {:-message %} %))
                    (apply merge)))

(defmacro composite
  ([validators interpret combine]
     {:pre [(symbol? combine) (symbol? interpret)]}
     `(composite ~validators ~interpret ~combine false))
  ([validators interpret combine force-multiple]
     {:pre [(symbol? combine) (symbol? interpret)]}
     (let [vs (map (fn [v] `(~interpret ~v)) validators)
           vs (cons 'list vs)
           vals (gensym "vals")
           c `{:validators ~vals
               :-method (symbol ~(str combine))}]
       `(let [~vals (map make-validator
                         (partition-runs :-method ~vs))]
          ~(if force-multiple
             c
             `(clojure.core/or (single ~vals) ~c))))))

(defmacro ^:validator and-all
  "Returns a single validator function which takes a single argument
  and calls all the given validators on it. If all the validators
  pass, it returns nil, otherwise it returns a sequence of errors."
  [& validators]
  `(composite ~validators interpret-is r/and-all))

(defmacro ^:validator and
  "Returns the conjunction of validator functions. The returned
  function takes a single argument and calls all the validator
  functions on it. If all the validations pass, it returns nil.
  If any validation fails, short-circuits and returns a sequence
  of errors, does not run the remaining validations."
  [& validators]
  `(composite ~validators interpret-is r/and))

(defmacro ^:validator? or
  "Returns the disjunction of validator functions. The returned
  function takes a single argument and calls all the validator
  functions on it. If at least one of the validations pass, it
  short-circuits and returns nil. If all validations fail, returns a
  sequence of errors."
  [& validators]
  `(composite ~validators interpret-is r/or))

(defmacro ^:validator ->> [& validators]
  `(composite ~validators interpret-as r/thread))

(defmacro ^:validator comp [& validators]
  `(let [v# (->> ~@validators)]
     (update-in v# [:validators] reverse)))

(defmacro ^:validator every
  "Returns a validator function that applies the validators to each
  element of the input collection."
  [& validators]
  `(composite ~validators interpret-is r/every true))

(defmacro ^:validator are
  "Returns a validator which will call (predicate ~@args input).
  predicate must be a symbol."
  [predicate & args]
  {:pre [(symbol? predicate)]}
  `(assoc (document-partial-% ~predicate ~@args)
     :-method 'r/are))

(def always-true {:-method `r/always-true})

(defn- to-match-clause [[predicate then]]
  [(if (= predicate :else)
     `always-true
     `(interpret-is ~predicate))
   `(interpret-as ~then)])

(defmacro ^:validator cond
  "Returns a validator function that checks multiple conditions. Each
  clause is a pair of a predicate and a validator.  Like cond, you
  can put :else as the last predicate.  If you fail a clause, you
  get back that claus's validator as a failure.  If no clauses match
  you get back the root clause."
  [& clauses]
  (let [validators (map to-match-clause (partition 2 clauses))]
    `{:clauses (list ~@validators)
      :-method `r/cond}))

(defmacro ^:validator when
  "Returns a validator that only checks the validators
  when predicate validates."
  [predicate & validators]
  `{:then (and ~@validators)
    :-method `r/when
    :predicate (interpret-is ~predicate)})

;;; Invocation patterns

(defmacro assert-valid
  "Tests the value of expr with validator. If it passes, returns the
  value. If not, throws an exception with validation information
  attached."
  [expr validator]
  `(let [v# ~expr]
     (let [result# (r/validate ~validator v#)]
       (if (r/valid? result#)
         (:result result#)
         (throw (ex-info "Validation failed"
                         {:errors (:errors result#)
                          :expr '~expr
                          :value v#
                          :line ~(:line (meta &form))
                          :file ~*file*}))))))
