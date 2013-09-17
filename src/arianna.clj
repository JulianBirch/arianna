(ns arianna
  (:refer-clojure :exclude [and comp cond count keys
                            or range vals when])
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.string :as s]
            [spyscope.core]))

;;; Core validation

; status is either :ok or :error (:pending may come later)

(def ^:dynamic *enable-protect-exception* true)

(defrecord ValidationResult [status result errors input])

(defn reduce-
  ([f coll] (let [v (reduce f coll)]
              (if (reduced? v) @v v)))
  ([f val coll] (let [v (reduce f val coll)]
                  (if (reduced? v) @v v))))

(defn report-failure
  ([this input result]
     (->ValidationResult :error result
                         (list {:validator this :value result})
                         input))
  ([this value] (report-failure this value value))
  ([value] (report-failure nil value value)))

(defn- add-exception [[error] exception]
  (list (assoc error :exception exception)))

(defprotocol IValidator
  "Validator abstraction"
  (validate- [this value] "Evaluates the validator."))

(defn validate [validator value]
  (if *enable-protect-exception*
      (try (validate- validator value)
           (catch Exception exception
             (update-in (report-failure validator value)
                        [:errors] add-exception exception)))
      (validate- validator value)))

(defn valid?
  "Returns true if value passes the validator."
  ([{:keys [status]}] (= :ok status))
  ([value validator] (valid? (validate validator value))))

(defn report-success
  ([value] (->ValidationResult :ok value nil value))
  ([input result] (->ValidationResult :ok result nil input)))

(defn test-validator [predicate this value]
  (if (predicate value)
    (report-success value)
    (report-failure this value)))

(defrecord PredicateValidator [predicate]
  IValidator
  (validate- [this value] (test-validator predicate this value)))

(defrecord FnValidator [f]
  IValidator
  (validate- [this value] (f this value)))

(defn enhance-error [this result]
  (update-in result [:errors]
             (fn [e] (map #(assoc % :validator this) e))))

(defn projection-validation-method [projection fail]
  (fn [this value]
    (let [result (projection value)]
      (if (clojure.core/and (map? result)
                            (contains? result :status))
        (if (valid? result)
          result
          (enhance-error this result))
        (if (= result fail)
          (report-failure this value result)
          (report-success value result))))))

(defn to-transform-fn [v]
  (clojure.core/cond
   (vector? v) (fn lookup [input] (get-in input v ::missing))
   (keyword? v) (fn lookup [input] (get input v ::missing))
   :else v))

(defn transform
  ([v] (transform v nil))
  ([v fail]
     (-> (to-transform-fn v)
         (projection-validation-method fail)
         ->FnValidator
         (assoc :projection v))))

(defn is-valid-empty [rules value]
  (clojure.core/or
   (if (contains? rules :missing)
     (= value ::missing))
   (if (contains? rules :nil)
     (nil? value))
   (clojure.core/and
    (contains? rules :blank)
    (string? value)
    (s/blank? value))))

(defrecord EmptyValidator [rules]
  IValidator
  (validate- [this value]
    (test-validator (partial is-valid-empty rules)
                    this value)))

(defn- valid-projection? [proj]
  (clojure.core/or (symbol? proj)
                   (keyword? proj)
                   (clojure.core/and
                    (vector? proj)
                    (every? keyword? proj))))

(defn validator
  ([predicate] (->PredicateValidator predicate))
  ([predicate expected]
     (merge (validator predicate) expected)))

;;; Validator-creating macros

(defmacro is
  "Returns a validator function which will call (pred ~@args input).
  pred must be a symbol."
  ([pred]
     {:pre [(symbol? pred)]}
     `(validator ~pred {:expected '~&form}))
  ([pred & args]
     {:pre [(symbol? pred)]}
     `(validator (partial ~pred ~@args) {:expected '~&form})))

(defmacro is-not
  "Returns a validator function which will call
  (not (pred ~@args input)). pred must be a symbol."
  ([pred]
     {:pre [(symbol? pred)]}
     `(validator (complement ~pred) {:expected '~&form}))
  ([pred & args]
     {:pre [(symbol? pred)]}
     `(validator (complement (partial ~pred ~@args))
                 {:expected '~&form})))

;;; Validator combinators

(defrecord CompositeValidator [validators combine]
  IValidator
  (validate- [this value] (combine this value)))

(defmacro as
  "Returns a validator which will call (proj ~@args input).
  proj must be a symbol.  "
  ([proj]
     {:pre [(valid-projection? proj)]}
     `(assoc (transform ~proj) :projection '~&form))
  ([proj & args]
     {:pre [(valid-projection? proj)]}
     `(assoc (transform (partial ~proj ~@args))
        :projection '~&form)))

(defn to-validator [v]
  (clojure.core/cond
   (satisfies? IValidator v) v
   (ifn? v) (transform v)
   :else (throw (ex-info "Not coercible to validator." {:v v}))))

(defn- single
  ([list default]
     (if (nil? (next list))
       (first list)
       default))
  ([list] (single list nil)))

(defn composite [validators combine]
  (if-let [v (single validators)]
    (to-validator v)
    (->CompositeValidator (mapv to-validator validators)
                          combine)))

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

(defn make-and-combine [f]
  (fn and-combine [{:keys [validators]} value]
    (->> validators
         (map #(validate % value))
         (reduce- f (report-success value)))))

(def and-combine (make-and-combine and-f))
(def and-all-combine (make-and-combine and-all-f))

(defn and-all
  "Returns a single validator function which takes a single argument
  and calls all the given validators on it. If all the validators
  pass, it returns nil, otherwise it returns a sequence of errors."
  [& validators]
  (composite validators and-all-combine))

(defn and
  "Returns the conjunction of validator functions. The returned
  function takes a single argument and calls all the validator
  functions on it. If all the validations pass, it returns nil. If any
  validation fails, short-circuits and returns a sequence of errors,
  does not run the remaining validations."
  [& validators]
  (composite validators and-combine))

(defn or-f [previous current]
  (if (valid? current)
    (reduced current)
    (merge-errors previous current)))

(defn or-combine [{:keys [validators] :as this} value]
  (let [fail (->ValidationResult :error nil [] value)
        vr (->> validators
                (mapv #(validate % value))
                (reduce- or-f fail))]
    (if (valid? vr)
      vr
      (let [or-error {:validator this
                      :errors (:errors vr)
                      :input value}]
        (->ValidationResult :error
                            value
                            (list or-error)
                            value)))))

(defn or
  "Returns the disjunction of validator functions. The returned
  function takes a single argument and calls all the validator
  functions on it. If at least one of the validations pass, it
  short-circuits and returns nil. If all validations fail, returns a
  sequence of errors."
  [& validators]
  (assoc
    (composite validators or-combine)
    :expected :one-of))

(defn add-chain [validator {:keys [result] :as vr}]
  (update-in vr [:chain] #(conj (clojure.core/or % [])
                                {:validator validator
                                 :result result})))

(defn comp-f [{:keys [result chain] :as previous} validator]
  (let [vr0 (validate validator result)
        vr (add-chain validator vr0)]
    (if (valid? vr)
      vr
      (reduced vr))))

(defn comp-combine [{:keys [validators] :as this} value]
  (reduce- comp-f
           (assoc (report-success value)
             :chain [{:validator this :result value}])
           (reverse validators)))

(defn comp [& validators]
  (composite validators comp-combine))

;;; Embedding normal functions in validators

(defn call-fn
  "Returns a validator that calls f on the input value and
  then performs validation on the return value of f. If f throws an
  exception, validation fails. validators are validation functions
  combined as with 'and'."
  [f validator]
  (comp validator (transform f ::fail)))

(defmacro call
  "Returns a validation that calls the function named by
  symbol on the input value and then performs validation on the return
  value of the function. validators are combined as with 'and'."
  [sym & validators]
  {:pre [(symbol? sym)]}
  `(call-fn ~sym (and ~@validators)))

(defmacro keys
  "Returns a validation that performs validation on the keys
  of its input, which must be a map."
  [& validators]
  `(call clojure.core/keys ~@validators))

(defmacro vals
  "Returns a validation that performs validation on the values
  of its input, which must be a map."
  [& validators]
  `(call clojure.core/vals ~@validators))

(defmacro count
  "Returns a validation that performs validation on the result
  of calling count on its input."
  [& validators]
  `(call clojure.core/count ~@validators))

;;; Reaching into associative structures

(defn has
  "Returns a validator that checks for the presence of keys
  in a nested associative structure. ks is a sequence of keys."
  [ks]
  (assoc (transform ks ::missing)
    :keys ks
    :operation :has))

(defn if-in
  "Like 'in' but does not return an error if the structure does not
  contain the given keys."
  [ks & validators]
  (comp (or
         (->EmptyValidator #{:missing :nil :blank})
         (apply and validators))
        (transform ks ::fail)))

(defn in
  "Returns a composition of validator functions that operate on a value
  in nested associative structures. Reaches into the structure as with
  'get-in' where ks is a sequential collection of keys. Calls the
  'and'd validators on the value. If any validations fail, returns
  a map with errors and the keys.

  If the structure does not contain ks, returns an error. See also
  if-in."
  [ks & validators]
  (comp (apply and validators) (has ks)))

(defn every-validate [validators this input]
  (reduce- and-all-f (report-success input)
           (for [v validators
                 item input]
             (validate v item))))

(defn every
  "Returns a validator function that applies the validators to each
  element of the input collection."
  [& validators]
  (assoc (->FnValidator
          #(every-validate validators %1 %2))
    :operator :every))

(defn are-validate-fn [pred this input]
  (let [r (filterv (complement pred) input)]
    (if (empty? r)
      (report-success this input)
      (->ValidationResult :error input
                          (mapv #(hash-map :validator this
                                           :value %) r)
                          input))))
(defn are-validator
  ([predicate] (->FnValidator
                #(are-validate-fn predicate %1 %2)))
  ([predicate expected]
     (merge (are-validator predicate) expected)))

(defmacro are
  "Returns a validator function which will call (pred ~@args input)
  on each element of an input collection. pred must be a symbol."
  ([pred]
     {:pre [(symbol? pred)]}
     `(are-validator ~pred
                     {:expected '~&form}))
  ([pred & args]
     {:pre [(symbol? pred)]}
     `(are-validator (partial ~pred ~@args)
                     {:expected '~&form})))

(defn clause-matches [input [pred _]]
  (or (= pred :else)
      (valid? (validate pred input))))

(defn cond
  "Returns a validator function that checks multiple conditions. Each
  clause is a pair of a predicate and a validator. Optional last
  argument is a validator to run if none of the predicates returns
  true; otherwise the validation fails."
  [& clauses]
  (let [clauses (-> clauses
                    (partition 2)
                    (map (juxt validator transform)))]
    (->FnValidator
     (fn [this input]
       (if-let [clause (->> clauses
                            (filter (partial clause-matches input))
                            first)]
         (validate (second clause) input)
         (report-failure this input))))))

(defn when
  "Returns a validator function that only checks the validators
  when (pred input) is true."
  [pred & validators]
  (let [validator (apply and validators)]
    (to-validator
     (fn when-f [input]
       (if (pred input)
         (validate validator input)
         (report-success input))))))

;;; Validation functions

(defn in-range [min-incl max-excl value]
  "Returns a validator function that checks if its input is numeric
  and is between min, inclusive, and max, exclusive."
  (clojure.core/and (<= min-incl value) (< value max-excl)))

(defn within [min-incl max-incl value]
  "Returns a validator function that checks if its input is between
  min and max, both inclusive. Uses clojure.core/compare to compare
  values."
  (not (clojure.core/or (neg? (compare value min-incl))
                        (pos? (compare value max-incl)))))

(defmacro field-is [field pred & args]
  (enhance-error (in [field] `(is ~pred ~@args))
                 #(assoc % :field field)))

; http://stackoverflow.com/questions/2640169/whats-the-easiest-way-to-parse-numbers-in-clojure
(defn parse-decimal-number [s]
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (clojure.core/and s
           (re-find #"^\s*-?\d+\.?\d*([Ee]\+\d+|[Ee]-\d+|[Ee]\d+)?$" s))
    (edn/read-string (s/replace s #"^0+(\d)" "$1"))))

(defn as-decimal-number [s]
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