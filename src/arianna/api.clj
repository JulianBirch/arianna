(ns arianna.api
  (:refer-clojure :exclude [and comp cond or when ->>])
  (:require [arianna.runtime :as r]
            [arianna.methods :as m]
            [spyscope.core]
            [poppea :refer [document-partial-map capture-%
                            partial-invoke-% defn-curried]]))

(defn valid-projection? [proj]
  (clojure.core/or (keyword? proj)
                   (vector? proj)))

(defn strip-vector [p]
  (if (clojure.core/and (vector? p) (= 1 (count p)))
    (first p)
    p))
;;; Validator-creating macros

(defmacro ^:validator is
  "Returns a validator which will call `(predicate ~@args input)`.
   Can use clojure's % syntax like an anonymous function, so
   `(v/is < % 10)` will call `(< input 10)`.

   predicate must be a symbol.

   Example usages

       (v/is string?)
       (v/is < % 10)"
  [predicate & args]
  {:pre [(symbol? predicate)]}
  `(assoc ~(document-partial-map predicate capture-% args)
     :arianna/v `r/is))

(defmacro ^:validator is-optional
  "Returns a validator, like is, which will call
   (predicate ~@args input). The validation result is always true,
   but if the test is true the composite predicate will immediately
   short-circuit.  Supports % the same way as `is`.

   Example usage:
       (v/is-optional v/absent? #{:nil :blank})

   Which prevents evaluation of subsequent validators if the value
   is nil or blank.

   Although callable anywhere, it only really makes sense to use it
   within `v/->>`.

   predicate must be a symbol."
  [predicate & args]
  {:pre [(symbol? predicate)]}
  `(assoc ~(document-partial-map predicate capture-% args)
     :arianna/v `r/is-optional))

(defmacro ^:validator is-not
  "Returns a validator, like `is`,  which will call
  (not (predicate ~@args input)).  Supports % the same way as `is`.

   predicate must be a symbol."
  [predicate & args]
  {:pre [(symbol? predicate)]}
  `(assoc ~(document-partial-map predicate capture-% args)
     :arianna/v `r/is-not))

(defmacro ^:validator as
  "Returns a validator which will call `(projection ~@args input)`.
   This will fail if the returned value is equal to :arianna/fail.
   By default, this means it will fail if the returned value is nil.

       (v/as number)
       ;;; will fail if the input was not a number or a string
       ;;; representing a number

       (assoc (v/as inc) :arianna/fail 3)
       ;;; Will fail if the input was equal to 2

   Supports use of the `%` symbol, same as `is`."
  [projection & args]
  {:pre [(symbol? projection)]}
  `(assoc ~(document-partial-map projection capture-% args)
     :arianna/v `r/as))

(defn ^:validator as-key
  "Returns a validator which will look into a map.
   Takes a keyword or a vector.  as-key never fails and
   if the map doesn't have the key specified the result
   will be the special value :arianna/missing.  You can test
   for this with v/required, v/optional or
   (v/is v/absent? #{:missing}).  (The first two also test for
   nil and blank strings.)

   The validator has two keys: `:projection` which is the
   input parameter and `:default`, which is the value returned
   if the value is missing.

       (v/as-key :x)
       ;;; Returns :arianna/missing if :x is not present
       (assoc (v/as-key :x) :default 3)
       ;;; Returns 3 if :x is not present
       (v/->> :x {:default 3})
       ;;; Exactly the same as the last example

   Note that :projection will have
   the vector removed if it's a vector with one element.  (This
   makes reporting the results simpler later, since [:x] is
   functionally identical to :x."
  [projection]
  {:pre [(valid-projection? projection)]}
  (let [v (strip-vector projection)]
    (assoc
        (if (vector? v)
          (as get-in % v :arianna/missing)
          (as get % v :arianna/missing))
      :arianna/fail :arianna/never)))

(defn ^:validator has
  "`has` behaves the same as `as-key`, expect that if the
   `projection` does not have a value, the validator fails."
  [projection]
  {:pre [(valid-projection? projection)]}
  (let [v (strip-vector projection)]
    (if (vector? v)
      (is m/contains-in? % v)
      (is contains? % v))))

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

(defn interpret-is-fn [v]
  (if (valid-projection? v)
    (has v)
    v))

(defn interpret-as-fn [v]
  (if (valid-projection? v)
    (as-key v)
    v))

(defmacro interpret-internal [v as interpret-fn]
  (clojure.core/or
   (let [v (if (seq? v) v (list v))
         f (first v)]
     (if (enhancable? f)
       (if (predicate-symbol? f)
         `(is ~@v)
         `(~as ~@v))))
   `(~interpret-fn ~v)))

(defmacro interpret-is [v]
  `(interpret-internal ~v is interpret-is-fn))

(defmacro interpret-as [v]
  `(interpret-internal ~v as interpret-as-fn))

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
                    (map #(if (string? %) {:arianna/message %} %))
                    (apply merge)))

(defn to-validators
  [validators interpret]
  {:pre [(symbol? interpret)]}
  (let [vs (map (fn [v] `(~interpret ~v)) validators)]
    `(clojure.core/->> (list ~@vs)
                       (partition-runs :arianna/v)
                       (map make-validator))))

(defmacro composite
  ([validators interpret combine]
     {:pre [(symbol? combine)]}
     `(composite ~validators ~interpret ~combine false))
  ([validators interpret combine force-multiple]
     {:pre [(symbol? combine)]}
     (let [vals (gensym "vals")
           c `{:validators ~vals
               :arianna/v (symbol ~(str combine))}
           vs (to-validators validators interpret)]
       `(let [~vals ~vs]
          ~(if force-multiple
             c
             `(clojure.core/or (single ~vals) ~c))))))

(defmacro ^:validator and-all
  "Takes a sequence of validators and creates a validator that
   applies them all, failing but not terminating if one fails.
   It will return all validation errors, while `and` will only
   return the first.

   Uses the same interpretation rules as `and`."
  [& validators]
  `(composite ~validators interpret-is r/and-all))

(defmacro ^:validator and
  "Takes a sequence of validators and creates a validator that
   applies them all, terminating if one fails.

   `and` has similar interpretation rules to `->>`:

   * `string?` becomes `(v/is string?)`
   * `inc` becomes `(v/is inc)`
   * `:key` becomes `(v/has :key)`
   * `[\"City\" \"Zip\"] becomes `(v/as-key `[\"City\" \"Zip\"])

   Note the differences:
   * inc is treated as a test (since a transform would make no sense)
   * if `:key` cannot be found, it's treated as a validation failure."
  [& validators]
  `(composite ~validators interpret-is r/and))

(defmacro ^:validator? or
  "Takes a sequence of validators and creates a validator that
   applies them all, terminating if one succeeds.  If it does
   not succeed, it will have a validation error that keys the
   `or` validator and an `:errors` key that contains a list
   of all of the constituent errors.

   Uses the same interpretation rules as `and`."
  [& validators]
  `(composite ~validators interpret-is r/or))

(defmacro ^:validator ->>
  "Creates a validator that chains the result of one validator into
   the next.  Validators that are tests, such as `is` will have
   output the same as their input.  Any failed validation or
   sucessful `is-optional` test will terminate the evaluation.  The
   validation error will contain a key `:chain` which contains the
   chain of validation results that led to the error.

   `->>` is a macro that has a number of interpretation rules that
   transform normal clojure syntax into validators.

   * `string?` becomes `(v/is string?)`
   * `inc` becomes `(v/as inc)`
   * `:key` becomes `(v/as-key :key)`
   * `[\"City\" \"Zip\"]` becomes `(v/as-key [\"City\" \"Zip\"])`

   `is` is used if the name of the function ends with a question
   mark or if it's a comparison operator in clojure.core.

   Chaining these together gives you the ability to do things like
   this:

       (v/->> :should-be-even v/required v/number even?)
       ;;; Input should be a map with a key :should-be-even
       ;;; that can be read as a number and that number is even.

       (v/->> :email v/optional v/email?)
       ;;; Input should be an email address or blank.

       (v/->> keys (v/are keyword?))
       ;;; Input should be a map where all the keys are keywords

   Next, if you follow a validator with a map, it will be merged
   into the validator.

       (v/->> :email {:default \"xjobcon@phx.cam.ac.uk\"})
       ;;; Provides a default email address
       ;;; see also `as-key` for more details

   Finally, strings are treated as maps with a `:arianna/message` key.
   These are used to provide human readable feedback by using
   stencil/mustache on the validation errors.

       (v/->> :email
              v/required \"You must provide an email.\"
              v/email? \"The input {{value}} doesn't appear to be an email address.\")
   "
  [& validators]
  `(composite ~validators interpret-as r/thread))

(defmacro ^:validator comp
  "Functionally identical to `->>` except that it evaluates
   the validators right to left instead of left to right."
  [& validators]
  `(let [v# (->> ~@validators)]
     (update-in v# [:validators] reverse)))

(defmacro ^:validator every
  "Returns a validator that applies the validators to each
  element of the input collection.

  Uses the same interpretation rules as `and`."
  [& validators]
  `(composite ~validators interpret-is r/every true))

(defmacro ^:validator are
  "Returns a validator which will call `(predicate ~@args x)` on
   every element `x` in the input value.

  `predicate` must be a symbol."
  [predicate & args]
  {:pre [(symbol? predicate)]}
  `(assoc ~(document-partial-map predicate capture-% args)
     :arianna/v 'r/are))

(def always-true
  "A validator that returns true for any input.  Only really useful
   at the repl."
  {:arianna/v `r/always-true})

(defmacro interpret-is-cond [predicate]
  (if (= predicate :else)
     `always-true
     `(interpret-is ~predicate)))

(defmacro ^:validator cond
  "Creates a validator function that checks multiple conditions. Each
  clause is a pair of a predicate and a validator.  Like `cond`, you
  can put `:else` as the last predicate.  If you fail a clause, you
  get back that clause's validator as a failure.  If no clauses match
  you get back the cond validator.

  Uses the same rules as `and` on the predicate clauses, and
  the same rules as `->>` on the validator clauses.  That said,
  adding a message to the predicate is pretty pointless."
  [& clauses]
  (let [p-clauses (to-validators clauses `interpret-is-cond)
        t-clauses (to-validators clauses `interpret-as)]
    `(let [c# (map #(if %1 %2 %3)
                   (iterate not true)
                   ~p-clauses
                   ~t-clauses)]
       {:clauses (partition 2 c#)
        :arianna/v `r/cond})))

(defmacro ^:validator when
  "Returns a validator that only checks the following validators
  when the first validates.  Interpets all validators using the
  same rules as and."
  [& validators]
  (let [v (to-validators validators `interpret-is)]
    `(let [[pred# then#] ~v]
       {:then then#
        :arianna/v `r/when
        :predicate pred#})))

;;; Invocation patterns

(defmacro assert-valid
  "Tests the value of `expr` with `validators`. If it passes,
  returns the value. If not, throws an exception with
  validation, line and file information attached.

  Applies `and-all` to validators.

  Note that this takes the `expr` first, making it suitable
  for use with `->`, and can be used to validate that long
  chains of `->` have usable intermediate values.

      (-> x
          inc
          (* 4)
          (v/assert-valid even?)
          (/ 2))
  "
  [expr & validators]
  `(let [val# ~expr
         v2# (and-all ~@validators)
         result# (r/validate v2# val#)]
     (if (r/valid? result#)
       (:result result#)
       (throw (ex-info "Validation failed"
                       {:errors (:errors result#)
                        :expr '~expr
                        :value val#
                        :line ~(:line (meta &form))
                        :file ~*file*})))))
