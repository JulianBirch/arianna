(ns arianna.validate
  "Compatibility namespace for comparison against Stuart Sierra's
   validate library."
  (:refer-clojure :exclude [count keys vals])
  (:require [arianna :as a]))

;;; Embedding normal functions in validators

;; This inner-call and transform is pretty ugly.
(defn inner-call [f value]
  (f value))

(defn ^:validator transform [f fail]
  (assoc (a/as inner-call f)
    :-fail fail))

(defn call-fn
  "Returns a validator that calls f on the input value and
  then performs validation on the return value of f. If f throws an
  exception, validation fails. validators are validation functions
  combined as with 'and'."
  [f validator]
  (a/comp validator (transform f ::fail)))

(defmacro call
  "Returns a validation that calls the function named by
  symbol on the input value and then performs validation on the
  return value of the function. validators are combined as
  with 'and'."
  [sym & validators]
  {:pre [(symbol? sym)]}
  `(call-fn ~sym (arianna/and ~@validators)))

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

(defmacro ^:validator if-in
  "Like 'in' but does not return an error if the structure does not
  contain the given keys."
  [ks & validators]
  `(a/->> (a/as-key ~ks)
          a/optional
          (a/and ~@validators)))

(defmacro ^:validator in
  "Returns a composition of validator functions that operate on a
  value in nested associative structures. Reaches into the
  structure as with 'get-in' where ks is a sequential collection
  of keys. Calls the 'and'd validators on the value. If any
  validations fail, returns a map with errors and the keys.

  If the structure does not contain ks, returns an error. See also
  if-in."
  [ks & validators]
  `(a/->> (a/has ~ks) (a/and ~@validators)))
