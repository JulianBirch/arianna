(ns arianna
  (:refer-clojure :exclude [and comp cond or when ->>])
  (:import [clojure.lang IPersistentVector ISeq Keyword])
  (:require [arianna.runtime :as r]
            [arianna.methods]
            [spyscope.core]
            [poppea :refer [document-partial-% partial-invoke-%
                            defn-curried]]
            [potemkin]))

(potemkin/import-vars
 [arianna.runtime

  validate
  validate-debug
  valid?
  message
  render-message]
 [arianna.methods

  present?
  all-empty-rules
  in-range?
  within?
  matches-regex?
  email?
  parse-decimal-number
  number])

(defn- valid-projection? [proj]
  (clojure.core/or (symbol? proj)
                   (keyword? proj)
                   (vector? proj)))
;;; Validator-creating macros

(defmacro ^:validator is
  "Returns a validator which will call (pred ~@args input).
  pred must be a symbol."
  [pred & args]
  {:pre [(symbol? pred)]}
  `(assoc (document-partial-% ~pred ~@args)
     :-method `r/is))

(defmacro ^:validator is-optional
  "Returns a validator which will call (pred ~@args input).
  pred must be a symbol."
  [pred & args]
  {:pre [(symbol? pred)]}
  `(assoc (document-partial-% ~pred ~@args)
     :-method `r/is-optional))

(defmacro ^:validator is-not
  "Returns a validator which will call
  (not (pred ~@args input)). pred must be a symbol."
  [pred & args]
  {:pre [(symbol? pred)]}
  `(assoc (document-partial-% ~pred ~@args)
     :-method `r/is-not))

(defmacro ^:validator as
  "Returns a validator which will call a projection.  "
  [projection & args]
  {:pre [(valid-projection? projection)]}
  (if (clojure.core/and (symbol? projection)
                        (not (keyword? `~projection))
                        (ifn? `~projection)
                        (resolve projection))
    `(assoc (document-partial-% ~projection ~@args)
       :-method `r/as)
    `{:-method `r/as-key
      :projection ~projection}))

;;; TODO: interpret-is should support has
(defmacro ^:validator has
  "Returns a validator which will call (proj ~@args input).
  proj must be a symbol.  "
  ([proj]
     {:pre [(valid-projection? proj)]}
     `{:-method `r/has-key
       :projection ~proj}))

(def optional (is-optional present? all-empty-rules))
(def required (is-not present? all-empty-rules))

;;; interpreting things as is or as

(defn- enhancable? [s]
  (if (symbol? s)
    (if-let [r (resolve s)]
      (if (-> r meta :validator not)
        (fn? @r)))
    (fn? s)))

(defmacro interpret-is [v]
  (clojure.core/or
   (let [v (if (seq? v) v [v])
         f (first v)]
     (if (enhancable? f)
       `(is ~@v)))
   v))

(defn- ends-with? [^String s ^String x]
  (.endsWith s x))

(def predicate-operators #{"<" "<=" "=" "==" ">=" ">"})

(defn predicate-symbol? [f]
  (let [n (name f)]
    (clojure.core/or
     (predicate-operators n)
     (ends-with? n "?"))) )

(defmacro interpret-as [v]
  (clojure.core/or
   (clojure.core/cond (vector? v) `(as ~v)
                      (keyword? v) `(as ~v)
                      :else (let [v (if (seq? v) v (list v))
                                  f (first v)]
                              (if (enhancable? f)
                                (if (predicate-symbol? f)
                                  `(is ~@v)
                                  `(as ~@v)))))
   v))

;;; composites

(defn- single
  ([list default]
     (if (nil? (next list))
       (first list)
       default))
  ([list] (single list nil)))

(defmacro composite-multiple [validators interpret combine]
  {:pre [(symbol? combine)]}
  (let [vs (map (fn [v] `(~interpret ~v)) validators)
        vs (cons 'list vs)
        s (str combine)]
    `{:validators ~vs
      :-method (symbol ~s)}))

(defmacro composite [validators interpret combine]
  (if-let [v (single validators)]
    `(~interpret ~v)
    `(composite-multiple ~validators ~interpret ~combine)))

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
  `(composite-multiple ~validators interpret-is r/every))

(defmacro ^:validator are
  "Returns a validator which will call (pred ~@args input).
  pred must be a symbol."
  ([pred & args]
     {:pre [(symbol? pred)]}
     ^:validator `(assoc (document-partial-% ~pred ~@args)
                    :-method 'r/are
                    :-required? true)))

(def always-true {:-method `r/always-true})

(defn- to-match-clause [[pred then]]
  [(if (= pred :else)
     `always-true
     `(interpret-is ~pred))
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
  when pred validates."
  [pred & validators]
  `{:then (and ~@validators)
    :-method `r/when
    :pred (interpret-is ~pred)})

;;; Invocation patterns

(defmacro assert-valid
  "Tests the value of expr with validator. If it passes, returns the
  value. If not, throws an exception with validation information
  attached."
  [expr validator]
  `(let [v# ~expr]
     (let [result# (validate ~validator v#)]
       (if (valid? result#)
         (:result result#)
         (throw (ex-info "Validation failed"
                         {:errors (:errors result#)
                          :expr '~expr
                          :value v#
                          :line ~(:line (meta &form))
                          :file ~*file*}))))))
