(ns arianna
  (:refer-clojure :exclude [and comp cond or when ->>])
  (:import [clojure.lang IPersistentVector ISeq Keyword])
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [spyscope.core]
            [poppea :refer [document-partial-% partial-invoke-%
                            defn-curried]]))

(def ^:dynamic *enable-protect-exception* true)

(defrecord ValidationError [validator value])

; status is either :ok or :error (:pending may come later)
(defrecord ValidationResult [status result errors input])

(defn- strip-reduced [v] (if (reduced? v) @v v))

(defn report-failure
  ([value] (report-failure nil value value))
  ([this value] (report-failure this value value))
  ([this input result]
     (->ValidationResult :error result
                         (list (ValidationError. this input))
                         input)))

(defn- add-exception [[error] exception]
  (list (assoc error :exception exception)))

(defn internal-validate [{:keys [-method] :as validator} value]
  {:pre (symbol? -method)}
  (if *enable-protect-exception*
    (try ((find-var -method) validator value)
           (catch Exception exception
             (update-in (report-failure validator value)
                        [:errors] add-exception exception)))
    ((find-var -method) validator value)))

(defn validate [validator value]
  (strip-reduced (internal-validate validator value)))

(defn validate-debug [validator value]
  (binding [*enable-protect-exception* false]
    (validate validator value)))

(defn valid?
  "Returns true if value passes the validator."
  ([{:keys [status]}] (= :ok status))
  ([value validator] (valid? (validate validator value))))

(defn report-success
  ([value] (->ValidationResult :ok value nil value))
  ([input result] (->ValidationResult :ok result nil input)))

(defn predicate-validate
  [{:keys [-required?] :as this} result value]
  (if -required?
    (if result
      (report-success value)
      (report-failure this value))
    (let [r (report-success value)]
      (if result (reduced r) r))))

(defn is-m [this value]
  (predicate-validate this
                      (partial-invoke-% this value)
                      value))

(defn is-not-m [this value]
  (predicate-validate this
                      (not (partial-invoke-% this value))
                      value))

(defn as-key-m [{:keys [projection default] :as this} value]
  (report-success (if (vector? projection)
                    (get-in value projection default)
                    (get value projection default))))

(defn has-key-m [{:keys [projection] :as this} value]
  (let [result (if (vector? projection)
                 (get-in value projection ::missing)
                 (get value projection ::missing))]
    (if (= ::missing result)
      (report-failure this value)
      (report-success value result))))

(defn enhance-error [this result]
  (update-in result [:errors]
             (fn [e] (map #(assoc % :validator this) e))))

(defn as-m [this value]
  (let [result (partial-invoke-% this value)]
    (if (instance? ValidationResult result)
      (if (valid? result)
        result
        (enhance-error this result))
      (if (= result (:-fail this))
        (report-failure this value result)
        (report-success value result)))))

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
     :-method 'arianna/is-m
     :-required? true))

(defmacro ^:validator is-optional
  "Returns a validator which will call (pred ~@args input).
  pred must be a symbol."
  [pred & args]
  {:pre [(symbol? pred)]}
  `(assoc (document-partial-% ~pred ~@args)
     :-method 'arianna/is-m
     :-required? false))

(defmacro ^:validator is-not
  "Returns a validator which will call
  (not (pred ~@args input)). pred must be a symbol."
  [pred & args]
  {:pre [(symbol? pred)]}
  `(assoc (document-partial-% ~pred ~@args)
     :-method 'arianna/is-not-m
     :-required? true))

(defmacro ^:validator as
  "Returns a validator which will call a projection.  "
  [projection & args]
  {:pre [(valid-projection? projection)]}
  (if (clojure.core/and (symbol? projection)
                        (not (keyword? `~projection))
                        (ifn? `~projection)
                        (resolve projection))
    `(assoc (document-partial-% ~projection ~@args)
       :-method 'arianna/as-m)
    `{:-method 'arianna/as-key-m
      :projection ~projection}))

(defmacro ^:validator has
  "Returns a validator which will call (proj ~@args input).
  proj must be a symbol.  "
  ([proj]
     {:pre [(valid-projection? proj)]}
     `{:-method 'arianna/has-key-m
       :projection ~proj}))

(defn present? [rules value]
  (clojure.core/or
   (if (contains? rules :missing)
     (= value ::missing))
   (if (contains? rules :nil)
     (nil? value))
   (clojure.core/and
    (contains? rules :blank)
    (string? value)
    (s/blank? value))))

(def all-empty-rules #{:missing :nil :blank})
(def optional (is-optional present? all-empty-rules))
(def required (is-not present? all-empty-rules))

;;; Validator combinators

(defn- single
  ([list default]
     (if (nil? (next list))
       (first list)
       default))
  ([list] (single list nil)))

(defn merge-errors [previous current]
  (if (valid? previous)
    current
    (update-in current [:errors]
               (partial concat (:errors previous)))))

(defn and-all-f [previous current]
  (if (valid? current)
    previous
    (merge-errors previous current)))

(defn and-f [previous current]
  (if (valid? current)
    current
    (reduced current)))

(defn-curried make-and-combine [f {:keys [validators]} value]
  (clojure.core/->> validators
                    (map #(internal-validate % value))
                    (reduce f (report-success value))
                    strip-reduced))

(def and-combine (make-and-combine and-f))
(def and-all-combine (make-and-combine and-all-f))

(defn enhancable? [s]
  (if (symbol? s)
    (if-let [r (resolve s)]
      (if (-> r meta :validator not)
        (fn? @r)))
    (fn? s)))

(defmacro interpret-is-2 [v]
  (let [f (first v)]
    (if (enhancable? f)
      `(is ~@v))))

(defmacro interpret-is [v]
  (clojure.core/or
   (let [v (if (seq? v) v [v])
         f (first v)]
     (if (enhancable? f)
       `(is ~@v)))
   v))

(defn ends-with? [^String s ^String x]
  (.endsWith s x))

(def predicate-operators #{"<" "<=" "=" "==" ">=" ">"})

(defn predicate-symbol? [f]
  (clojure.core/or
   (predicate-operators (name f))
   (ends-with? (name f) "?")) )

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
  `(composite ~validators interpret-is arianna/and-all-combine))

(defmacro ^:validator and
  "Returns the conjunction of validator functions. The returned
  function takes a single argument and calls all the validator
  functions on it. If all the validations pass, it returns nil. If any
  validation fails, short-circuits and returns a sequence of errors,
  does not run the remaining validations."
  [& validators]
  `(composite ~validators interpret-is arianna/and-combine))

(defn or-f [previous current]
  (if (valid? current)
    (reduced current)
    (merge-errors previous current)))

(defn or-combine [{:keys [validators] :as this} value]
  (let [fail (->ValidationResult :error nil [] value)
        vr (clojure.core/->> validators
                             (mapv #(internal-validate % value))
                             (reduce or-f fail)
                             strip-reduced)]
    (if (valid? vr)
      vr
      (let [or-error (assoc (ValidationError. this value)
                       :errors (:errors vr))]
        (->ValidationResult :error
                            value
                            (list or-error)
                            value)))))

(defmacro ^:validator? or
  "Returns the disjunction of validator functions. The returned
  function takes a single argument and calls all the validator
  functions on it. If at least one of the validations pass, it
  short-circuits and returns nil. If all validations fail, returns a
  sequence of errors."
  [& validators]
  `(composite ~validators interpret-is arianna/or-combine))

(defn apply-reduced [value f]
  (if (reduced? value)
    (reduced (f @value))
    (f value)))

(defn add-chain [validator {:keys [result] :as vr}]
  (update-in vr [:chain] #(conj (clojure.core/or % [])
                                {:validator validator
                                 :result result})))

(defn comp-f [{:keys [result] :as previous} validator]
  (let [vr0 (internal-validate validator result)
        vr (apply-reduced vr0 (partial add-chain validator))]
    (if (clojure.core/or (reduced? vr) (valid? vr))
      vr
      (reduced vr))))

(defn thread [{:keys [validators] :as this} value]
  (clojure.core/->>
   validators
   (reduce comp-f
           (assoc (report-success value)
             :chain [{:validator this :result value}]))
   strip-reduced))

(defmacro ^:validator ->> [& validators]
  `(composite ~validators interpret-as arianna/thread))

(defmacro ^:validator comp [& validators]
  `(let [v# (->> ~@validators)]
     (update-in v# [:validators] reverse)))

(defn every-validate [validators this input]
  (clojure.core/->> (for [v validators
                          item input]
                      (validate v item))
                    (reduce and-all-f (report-success input))
                    strip-reduced))

(defn every-m [{:keys [validators] :as this} value]
  (every-validate validators this value))

(defmacro ^:validator every
  "Returns a validator function that applies the validators to each
  element of the input collection."
  [& validators]
  `(composite-multiple ~validators interpret-is arianna/every-m))

(defn are-m [this input]
  (let [r (filterv #(not (partial-invoke-% this %)) input)]
    (if (empty? r)
      (report-success this input)
      (->ValidationResult :error
                          input
                          (mapv #(ValidationError. this %) r)
                          input))))

(defmacro ^:validator are
  "Returns a validator which will call (pred ~@args input).
  pred must be a symbol."
  ([pred & args]
     {:pre [(symbol? pred)]}
     ^:validator `(assoc (document-partial-% ~pred ~@args)
                    :-method 'arianna/are-m
                    :-required? true)))


(defn always-true-f [this input] (report-success input))

(def always-true {:-method `always-true-f})

(defn to-match-clause [[pred then]]
  [(if (= pred :else)
     `always-true
     `(interpret-is ~pred))
   `(interpret-as ~then)])

(defn-curried valid-clause [input clause]
  (valid? input (first clause)))

(defn cond-f [{:keys [clauses] :as this} input]
  (if-let [clause
           (first (filter (valid-clause input) clauses))]
    (validate (second clause) input)
    (report-failure this input)))

(defmacro ^:validator cond
  "Returns a validator function that checks multiple conditions. Each
  clause is a pair of a predicate and a validator.  Like cond, you
  can put :else as the last predicate.  If you fail a clause, you
  get back that claus's validator as a failure.  If no clauses match
  you get back the root clause."
  [& clauses]
  (let [validators (map to-match-clause (partition 2 clauses))]
    `{:clauses (list ~@validators)
      :-method 'cond-f}))

(defn when-f [{:keys [pred then]} input]
  (if (valid? input pred)
    (validate then input)
    (report-success input)))

(defmacro ^:validator when
  "Returns a validator that only checks the validators
  when pred validates."
  [pred & validators]
  `{:then (and ~@validators)
    :-method 'when-f
    :pred (interpret-is ~pred)})

;;; Validation functions

(defn in-range? [min-incl max-excl value]
  "Returns a validator function that checks if its input is numeric
  and is between min, inclusive, and max, exclusive."
  (clojure.core/and (<= min-incl value) (< value max-excl)))

(defn within? [min-incl max-incl value]
  "Returns a validator function that checks if its input is between
  min and max, both inclusive. Uses clojure.core/compare to compare
  values."
  (not (clojure.core/or (neg? (compare value min-incl))
                        (pos? (compare value max-incl)))))

                                        ; Direct rip from libnoir

(defn matches-regex?
  "Returns true if the string matches the given regular expression"
  [regex v]
  (boolean (re-matches regex v)))

(defn email?
  "Returns true if v is an email address"
  [v]
  (matches-regex? #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?" v))

; http://stackoverflow.com/questions/2640169/whats-the-easiest-way-to-parse-numbers-in-clojure

; Don't think core.typed can verify this
(defn parse-decimal-number [s]
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (clojure.core/and s
           (re-find #"^\s*-?\d+\.?\d*([Ee]\+\d+|[Ee]-\d+|[Ee]\d+)?$" s))
    (edn/read-string (s/replace s #"^0+(\d)" "$1"))))

(defn number [s]
  (if (number? s) s (parse-decimal-number s)))

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
