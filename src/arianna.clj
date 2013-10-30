(ns arianna
  (:refer-clojure :exclude [and comp cond or when ->>])
  (:import [clojure.lang IPersistentVector ISeq Keyword])
  (:require [arianna runtime methods api]
            [spyscope.core]

            [potemkin]))

(potemkin/import-vars
 [arianna.runtime

  validate
  validate-debug
  valid?
  render-message
  field
  summarize]
 [arianna.methods

  absent?
  in-range?
  within?
  regex-match?
  email?
  parse-decimal-number
  number
  contains-in?]
 [arianna.api

  is
  is-optional
  is-not
  as
  as-key
  has
  are

  and-all
  and
  or
  ->>
  comp
  every
  always-true
  cond
  when

  assert-valid])

(def optional
  "Shorthand for `(v/is-optional absent? #{:missing :nil :blank})`.

   Example usage:
       (v/->> :email v/optional v/email?)

   A rule that says that either `:email` is a valid email or
   it's not present."
  (is-optional absent? #{:missing :nil :blank}))

(def required
  "Shorthand for `(v/is-not absent? #{:missing :nil :blank})`.

   Example usage:
       (v/->> :email v/required v/email?)

   A rule that says that `:email` is a valid email and is present."
  (is-not absent? #{:missing :nil :blank}))
