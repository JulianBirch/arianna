# arianna

Arianna is a Clojure data validation library. Specifically, it's a fork of Stuart Sierra's excellent [validate](https://github.com/stuartsierra/validate) library.  That said, I'm not averse to ripping off code and good ideas from other validation libraries including [Validateur](https://github.com/michaelklishin/validateur) and [libnoir](https://github.com/noir-clojure/lib-noir).

**ALPHA code:** Even with ripping off Stuart Sierra's design, this is a young library.  Pretty much the entire API is subject to change
without warning in subsequent 0.x releases.

## Description

A *validator* is a data structure.  When you call `(v/validate validator input)` it will return a *validation result*, a record with the following fields
* `:result` The result of the transformation, or nil if the validator is a straight predicate.
* `:input` The input to the validator
* `:status` Either `:ok` or `:error`.  You're better off calling `(v/valid? validation-result)` than using this directly.
* `:errors` A sequence of *validation errors*.

Validation errors are
* `:validator` The validator that failed.
* `:value` The value that caused the failure
* `:chain` The execution chain that gave rise to the value.

Most of this library consists of tools to create and compose
validators.

Note that you can compose validators, so the result of one validator becomes the input to the next.

## Goals

The goal of this library is to construct validators which:

1. Are fully composable
2. Do not assume anything about the input structure
   (e.g. that it is a map)
3. Return data structures (for internationalization or further
   processing) instead of strings
4. Return a description of what failed and why
5. Validators are data
6. Provides a full trace of the cause of a failure.
7. Support web form validation naturally

Note that the passing branch in arianna is slower than the original validate library.

### Unachieved goals

1. Support ClojureScript
2. Support asynchronous validation

Obviously, async is likely to result in a serious API change.

## Releases and Dependency Information

Available on Clojars

```clj
[net.colourcoding/arianna "0.1.2"]
```

## Example Usage

```clj
    (require '[arianna :as v])
```

### Creating Validation Functions

N.B.  It's well worth checking out the unit tests to see how the library is used.

The `is` and `is-not` macros transform any boolean predicate function into a validator that contains the original expression in the `:expected` key:

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

    (:expected number-validator)
    ;;=> (v/is number?)
```

The `is` macro only works on symbols. You can transform an abitrary
boolean predicate function into a validator with the
`validator` function. `validator` takes a predicate and an optional
validator description, which must be a map and will be merged into
the validator (and hence accessible via the errors).

```clj
    (def under-10
      (v/validator #(< % 10) {:error "must be less than 10"}))

    (:error under-10)
    ;;=> "must be less than 10"

    (under-10 42)
    ;;=> {:status :error
          :result nil
          :errors [{:validator under-10 :value nil}]
          :input "hi}
```

### Transform Validations

There's an equivalent macro for transform validations: `as`.
The equivalent of `validator` is `transform`.
`transform` takes an optional second parameter, which is the
value a transform returns when it fails.  The default is nil.

```clj
    (v/validate (v/as v/as-decimal-number) "4")
    ;;=> {:status :ok, :result 4, :errors nil, :input "4"}
```

### Assertions

Normally you would not call a validator directly but instead
pass it to the `assert-valid` macro, which throws an exception if the
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

There is also a function, `valid?`, that returns true if the
validation passes or false if it does not:

```clj
    (v/valid? "hello" (v/is string?))
    ;;=> true
```

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

It also supports `or` and `comp`, which work as you might imagine.
Parameters that aren't validators get `transform` called on them.

### Branching

There are two branching constructs: `cond` and `when`.  `cond` takes
pairs of test and transform validators.

### Looking Inside Collections

The `every` function transforms a simple validation function into a
validation function that operates on each element of a collection:

```clj
    ((v/every (v/is even?)) [4 3 8 15])
    ;;=> ({:value 3, etc}
    ;;    {:value 15, etc})
```

The `are` macro is shorthand for `every` and `is`.

```clj
    ((v/are even?) [4 3 8 15])
```

For map-like collections, you can use `keys` and `vals` to apply
validations to the keys and values of the map, respectively:

```clj
    (def are-string (v/are string?))
    (def simple-map
      (v/and (v/keys (v/are keyword?)) (v/vals are-string)))


    (v/validate simple-map {:a "one", :b 2})
    ;;=> {:errors ({:expected are-string, :value 2})
```

You can also use `count` to validate facts about the number of
elements in a collection:

```clj
    ((v/count (v/validator #(< % 4))) [:a :b :c :d :e])
```

(This error message needs work.)

More generally, you can call any function on the input and perform
additional validation on the return value of that function with the
`call` macro and `call-fn` function.

### Looking Inside Maps

When you know that certain keys should be present in a map, you can
check that their values pass additional validation checks. The `in`
function takes a vector of keys and one or more validation functions.
It returns a validation function that navigates into the map as with
Clojure's `get-in` function and runs the validation functions on the
value.

```clj
    (def john {:name "John Doe", :address {:city "Baltimore"}})

    (v/valid? (v/in [:address :city] (v/is string?)) john)
    ;;=> true

    (v/valid? (v/in [:address :zip] (v/is integer?)) john)
    ;;=> false
```

With `in`, the map must contain the given keys or it fails the
validation. An alternate form, `if-in`, allows the validation to pass
if any keys are missing.

```clj
    (v/valid? (v/if-in [:address :zip] (v/is integer?)) john)
    ;;=> true
```

(Optionals need more work.)

## Development and Contributing

All pull requests, issues and conversations are welcome, but I do
ask that any changes come with tests that demonstrate the original problem.

## FAQ

### Why is it called Arianna?
[Listen](http://www.youtube.com/watch?v=VZc2npAmQXM)

### Why yet another validation library?

Honestly, to scratch an itch.  However, the biggest difference is that validate and Validateur represent validators as functions.  This in turn means that feedback is ad-hoc.  Arianna is me trying to come up with a regular way of dealing with feedback.  It's not completely successful at the present time.  Combining the ability to use any function and still have things be data driven is tricky.

libnoir has some useful functionality arianna currently lacks.  I'm planning to rectify that by stealing the code.

## Copyright and License

Copyright (c) Stuart Sierra, Julian Birch 2013. All rights reserved.
The use and distribution terms for this software are covered by
the Eclipse Public License 1.0
(http://opensource.org/licenses/eclipse-1.0.php) which can be
found in the file epl-v10.html at the root of this distribution.
By using this software in any fashion, you are agreeing to be
bound by the terms of this license. You must not remove this
notice, or any other, from this software.
