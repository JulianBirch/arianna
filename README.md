# arianna

Arianna is a Clojure data validation library. Specifically, it's a fork of Stuart Sierra's excellent [validate](https://github.com/stuartsierra/validate) library.  That said, I'm not averse to ripping off code and good ideas from other validation libraries including [Validateur](https://github.com/michaelklishin/validateur) and [libnoir](https://github.com/noir-clojure/lib-noir).

Most of this library consists of tools to create and compose
validators.  Actual validation logic is just handled by normal
Clojure functions with no special rules so, for instance, if
you want to check something is a string, you just use `string?`.
If you want to check the keys of a map, you just call `keys`.  There
are a couple of useful functions such as `v/email?` for common
cases.

Note that you can compose validators, so the result of one validator becomes the input to the next.

**ALPHA code:** Even with ripping off Stuart Sierra's design, this is a young library.  Pretty much the entire API changed between 0.1 and 0.2, although the philosphy has not.  It's just that 0.2 achieves its aims better than 0.1.

## Releases and Dependency Information

Available on Clojars

```clj
    [net.colourcoding/arianna "0.2.1"]
```

You can read the [full API docs](http://julianbirch.github.io/arianna/arianna.html), but I'd recommend finishing this readme first.

## Example Usage

Here's a fully worked example (we'll explain how it works later)

```clj
    (require '[arianna :as v])

    (v/summarize

      (v/and-all (v/->> keys (v/are keyword?))
                 (v/->> values (v/are string?))
                 (v/->> :email v/optional
                        email? "{{value}} is not a valid email.")
                 (v/->> :name v/require "You must enter a name.")
                 (v/->> :message v/require "You must enter a message."
                        count (> % 20) "Message must be longer than {{validator.y}} characters."))
         input)
```

will give a result something like

```clj
{:email ["xxx is not a valid email"]
 :name ["You must enter a name"]
 :message ["Message must be longer than 20 characters."]}
```

Or `nil` if the input passes (making the result suitable for use within `if-let`).

If you want the full data structure for what failed, you call
`v/validate` instead.  You can call `v/valid?` on that result
to check if it passes.

The following is a quick introduction to the libraries.  More
details are available in the API docs.

### Creating Validation Functions

N.B.  It's well worth checking out the unit tests to see how the library is used.

The primitive validation macros are: `is`, `is-not` and `are`.  They transform boolean predicate function into a validator:

```clj
    (def number-validator (v/is number?))

    (v/validate number-validator 42)
    ;;=> {:status :ok
          :result 42
          :errors nil
          :input 42}

    (number-validator "hi")
    ;;=> {:status :error
          :result nil
          :errors [{:validator number-validator :value "hi"}]
          :input "hi"}
```

If the function doesn't take the input as the last parameter, you
can indicate its position with `%`.

```
   (defn less-than-10 (v/is < % 10))
```

`has` takes a keyword or a vector, `k`, and expands to
`(v/is contains? % k)` or `(v/is contains-in? % k)` as appropriate.
Single element vectors are stripped to their value.

Finally, there is `is-optional`, which is only really useful in
conjunction with `v/->>`.  There's more information in the API
docs.

### Transform Validations

There's an equivalent macro for transform validations: `as`.

```clj
    (v/validate (v/as v/number) "4")
    ;;=> {:status :ok, :result 4, :errors nil, :input "4"}
```

There is also an equivalent of `has`, `as-key`.  `as-key` always
passes, and returns the special value `:arianna/missing` if the
key is not present.

### Combining Validation Functions

The `and` function combines multiple validation functions into one.
This is not the same as `clojure.core/and`, but it does short-circuit
the same way:

```clj
    (def odd-integer
      (v/and (v/is integer?)
             (v/is odd?)))

    (odd-integer 4)
    ;;=> ({:expected (v/is odd?), :value 4})

    (odd-integer 4.0)
    ;;=> ({:expected (v/is integer?), :value 4.0})
```

`and-all` does the same, but does not short circuit.  This is better
for when you want to provide more feedback and less useful when later
feedback would be redundant.

There's also a short syntax which makes it a lot more convenient:

```clj
(def odd-integer (v/and integer? odd?))
```

You can also follow validators with strings to provide them with
human-readable feedback through `v/summarize`.

```clj
(def odd-integer (v/and integer? "Should be an integer."
                        odd? "Should be odd."))
```

It also supports `or`, which works as you might imagine.

### Threading

Here's the interesting bit: you can compose transform validators.

```
  (v/->> :email v/required v/email?)
```

is equal to

```
  (v/->> (v/as-key :email)
         (v/is-not v/absent? #{:missing :nil :blank})
         (v/is v/email?))
```

### Validating Collectinos

The `every` function transforms a list of validators into a
validator that operates on each element of a collection:

```clj
    (v/validate (v/every even?) [4 3 8 15])
```

Will give two errors, one keyed by 3 and one by 15.

The `are` macro is a synonym for `every`.

`are` composes effectively with the threading macro to give more capabilities.  For map-like collections, you can thread `keys` and `vals` to apply validations to the keys and values of the map, respectively:

```clj
    (def are-string (v/are string?))
    (def simple-map
      (v/and (v/->> keys (v/are keyword?))
             (v/->> vals are-string)))


    (v/validate simple-map {:a "one", :b 2})
    ;;=> {:errors ({:validator (v/is string?), :value 2})
```

You can also use `count` to validate facts about the number of
elements in a collection:

```clj
    (v/validate (v/->> count (< % 4))) [:a :b :c :d :e])
```

or, for that matter, strings.

```clj
    (v/validate (/->> count (in-range? 3 10)) "password")
```

### Branching

There are two branching constructs: `cond` and `when`.  `cond` takes
pairs of test and transform validators.

## Results of validate

A *validator* is a data structure.  When you call `(v/validate validator input)` it will return a *validation result*, a record with the following fields
* `:result` The result of the transformation, or nil if the validator is a straight predicate.
* `:input` The input to the validator
* `:status` Either `:ok` or `:error`.  You're better off calling `(v/valid? validation-result)` than using this directly.
* `:errors` A sequence of *validation errors*.

Validation errors are
* `:validator` The validator that failed.
* `:value` The value that caused the failure
* `:chain` The execution chain that gave rise to the value.  This a sequence of validation results assoced with the :validator that gave rise to them.

### Exceptions

Exceptions will be caught by `v/validate` and `v/summarize` and
treated as validation failures.  The validation error will have
a key `:exception` which contains the original exception.

`v/debug-validate` doesn't catch the exceptions and is useful
on the repl.

## Human readable feedback

Let's say you've got a map

```clj
(defn u {:user "moomin" :password "zog"})
```

and a validator
```clj
    (def pass (v/->> :password
                     count
                     (in-range? 6 10)
                     "Password length must be between {{validator.min-incl}} characters and {{validator.max-excl}}."))
```

The string is associated with the previous validator.  The messages
are mustache templates applied to validation errors keyed by the
validator.  `{{value}}` is the value entered into the validator.
`{{validator.min-incl}}` is the `min-incl` parameter to the
function.  If that doesn't make you choose decent parameter
names, nothing will.

The `v/summarize` function can be used to process these strings:

```
    (v/summarize pass u)
    ;;; {:password ["Password length must between 6 and 10."]}
```

You can associate any information you like with the validator by
either a) just calling assoc on a primitive or b) putting
a hash-map after it in a composite.  If you want to override the
key, you set the `:arianna/field` key.  This is pretty useful
where the system can't deduce the field.

```
    (defn matching-fields? [f1 f2 input]
        (= (get f1 input) (get f2 input)))

    (def repeat-email
         (v/->> (matching-fields? :email :repeat-email)
                "Email and Repeat Email fields don't match."
                {:arianna/field :repeat-email}))
```

## Assertions

If you need to validate internal data, rather than business data, the `assert-valid` macro can be useful.  It throws an exception if the
validation fails:

```clj
    (v/assert-valid (/ 22.0 7.0) (v/is integer?))
    ;; #<ExceptionInfo clojure.lang.ExceptionInfo: Validation failed ...>
```

Get the error messages out from `ex-data`:

```clj
    (ex-data *e)
    ;;=> {:errors ({:value 3.142857142857143,
    ;;              :validator (v/is integer?)}),
    ;;    :line 1,
    ;;    :expr (/ 22.0 7.0),
    ;;    :value 3.142857142857143,
    ;;    :file "NO_SOURCE_PATH"}
```

When the validation passes, `assert-valid` returns the input value,
making it suitable for pipelining:

```clj
    (-> (rand-int 100)
        (* 2)
        inc
        (v/assert-valid (v/is odd?)))
    ;;=> 145
```

## Development and Contributing

All pull requests, issues and conversations are welcome, but I do
ask that any changes come with tests that demonstrate the original problem.

## FAQ

### Why is it called Arianna?
[Listen](http://www.youtube.com/watch?v=VZc2npAmQXM)

### What's the aim of the library?

The library aims to allow to

1. use normal clojure functions to perform validation
2. give you a full trace of any failures
3. validate anything, but provide useful features for validating
maps from web forms and json posts.

What it explicitly doesn't want to be, is a type system like [Schema](https://github.com/Prismatic/schema).

### Unachieved goals

1. Support ClojureScript
2. Support asynchronous validation

Obviously, async is likely to result in a serious API change.

### Why yet another validation library?

Honestly, to scratch an itch.  However, the biggest difference is that validate and Validateur represent validators as functions.  This in turn means that feedback is ad-hoc.  Arianna is me trying to come up with a regular way of dealing with feedback.  It's not completely successful at the present time.  Combining the ability to use any function and still have things be data driven is tricky.

libnoir has some useful functionality arianna currently lacks.  I'm planning to rectify that by stealing the code.

### What's the arianna.validate namespace?

It contains compatibility functions with "validate".  It's mostly useful to see how to move between one and the other.  All the functions are unit tested, so they can be used, but I think the native syntax is preferable.

## Copyright and License

Copyright (c) Stuart Sierra, Julian Birch 2013. All rights reserved.
The use and distribution terms for this software are covered by
the Eclipse Public License 1.0
(http://opensource.org/licenses/eclipse-1.0.php) which can be
found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be
bound by the terms of this license. You must not remove this
notice, or any other, from this software.
