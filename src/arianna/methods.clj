(ns arianna.methods
  (:require [clojure.edn :as edn]
            [clojure.string :as s]))

(defn absent?
  "Method that is used to determine if the `value` is absent in
   some way.  Rules should be a set.  The possible values of the
   set are:

   * `:nil`: return true if the value is `nil`
   * `:blank`: return true if the value is an empty string
   * `:missing`: return true if the value is `:arianna/missing`

   The last, `:missing` is a special value returned by `as-key`
   when it fails to find the key."
  [rules value]
  (clojure.core/or
   (if (contains? rules :missing)
     (= value :arianna/missing))
   (if (contains? rules :nil)
     (nil? value))
   (clojure.core/and
    (contains? rules :blank)
    (string? value)
    (s/blank? value))))

;;; Validation functions

(defn in-range?
  "Method that checks if its `value` is numeric
   and is between min, inclusive, and max, exclusive."
  [min-incl max-excl value]
  (clojure.core/and (<= min-incl value) (< value max-excl)))

(defn within?
  "Method that checks if its `value` is between
   min and max, both inclusive. Uses clojure.core/compare to compare
   values."
  [min-incl max-incl value]
  (not (clojure.core/or (neg? (compare value min-incl))
                        (pos? (compare value max-incl)))))

;;; Direct rip from libnoir

(defn matches-regex?
  "Returns true if the string `v` matches the given regular
   expression."
  [regex v]
  (boolean (re-matches regex v)))

(defn email?
  "Returns true if `v` is an email address."
  [v]
  (matches-regex? #"(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?" v))

; http://stackoverflow.com/questions/2640169/whats-the-easiest-way-to-parse-numbers-in-clojure

; Don't think core.typed can verify this
(defn parse-decimal-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (clojure.core/and s
           (re-find #"^\s*-?\d+\.?\d*([Ee]\+\d+|[Ee]-\d+|[Ee]\d+)?$" s))
    (edn/read-string (s/replace s #"^0+(\d)" "$1"))))

(defn number
  "Transform method that tries to convert `s` to a number.
   Returns nil on failure."
  [s]
  (if (number? s) s (parse-decimal-number s)))
