(ns arianna.validate
  "Compatibility namespace for comparison against Stuart Sierra's
   validate library."
  (:refer-clojure :exclude [count keys vals])
  (:require [arianna :as a]))

;;; Embedding normal functions in validators

(defn call-fn
  "Returns a validator that calls f on the input value and
  then performs validation on the return value of f. If f throws an
  exception, validation fails. validators are validation functions
  combined as with 'and'."
  [f validator]
  (a/comp validator (a/transform f ::fail)))

(defmacro call
  "Returns a validation that calls the function named by
  symbol on the input value and then performs validation on the
  return value of the function. validators are combined as
  with 'and'."
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
  (assoc (a/transform ks :arianna/missing) :operation :has))

(defn if-in
  "Like 'in' but does not return an error if the structure does not
  contain the given keys."
  [ks & validators]
  (a/->> ks
         a/optional
         (apply a/and validators)))

(defn in
  "Returns a composition of validator functions that operate on a
  value in nested associative structures. Reaches into the
  structure as with 'get-in' where ks is a sequential collection
  of keys. Calls the 'and'd validators on the value. If any
  validations fail, returns a map with errors and the keys.

  If the structure does not contain ks, returns an error. See also
  if-in."
  [ks & validators]
  (a/->> (has ks) (apply a/and validators)))
