(ns arianna.methods
  (:require [clojure.edn :as edn]
            [clojure.string :as s]))

(defn absent? [rules value]
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

;;; Direct rip from libnoir

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
