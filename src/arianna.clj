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
  summarize]
 [arianna.methods

  absent?
  in-range?
  within?
  matches-regex?
  email?
  parse-decimal-number
  number]
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

(def optional (is-optional absent? #{:missing :nil :blank}))
(def required (is-not absent? #{:missing :nil :blank}))
