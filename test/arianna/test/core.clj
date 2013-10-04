(ns arianna.test.core
  (:require [arianna :as v]
            [arianna.validate :as av]
            [clojure.test :refer :all])
  (:import [arianna ValidationError ValidationResult]))

(def number-validator (v/is number?))

(deftest simple
  (is (v/validate number-validator 42)
      {:status :ok
       :result 42
       :errors nil
       :input 42})
  (is (v/validate number-validator "hi")
      {:status :error
       :result nil
       :errors [(ValidationError. number-validator "hi")]
       :input "hi"})
  (is (v/valid? "Baltimore" (v/is string?))
      "Baltimore is a string."))

(def under-10 (v/validator #(< % 10) {:error "must be less than 10"}))

(defn get-errors [validator input]
  (:errors (v/validate validator input)))

(deftest u10
  (is (= (:error under-10) "must be less than 10")
      "Validator should have an error property.")
  (is (= (-> (get-errors under-10 42)
             first
             :validator
             :error) "must be less than 10")
      "Should be possible to extract out provided error."))

(deftest assert-valid-
  (is (thrown? java.lang.Exception
               (v/assert-valid (/ 22.0 7.0) (v/is integer?))
               "Failed assert should throw."))
  (is (= (-> 30 (* 2) inc (v/assert-valid (v/is odd?)))
         61)
      "Assert valid should be equivalent to identity when valid"))

(def is-integer (v/is integer?))
(def is-float (v/is float?))
(def is-odd (v/is odd?))
(def is-even (v/is even?))

(def is-even-optional (v/optional-validator even?))

(deftest optional-tests
  (is (instance? ValidationResult (v/validate is-even-optional 3)))
  (is (instance? ValidationResult (v/validate is-even-optional 4)))
  (is (instance? ValidationResult @(v/internal-validate is-even-optional 4))))

(def odd-integer (v/and is-integer is-odd))

(def are-even (v/are even?))

(deftest composites
  (is (v/valid? "Baltimore" (v/and (v/is string?)))
      "and composite of single item works.")
  (is (v/valid? "Baltimore" (v/and (v/is string?) (v/is string?)))
      "and composite of two items works.")
  (is (= (get-errors odd-integer 4)
         [(ValidationError. is-odd 4)]))
  (is (= (get-errors odd-integer 4.0)
         [(ValidationError. is-integer 4.0)]))
  (is (= (get-errors (v/every is-even) [4 3 8 15])
         [(ValidationError. is-even 3)
          (ValidationError. is-even 15)]))
  (is (= [(ValidationError. are-even 3)
          (ValidationError. are-even 15)]
         (get-errors are-even [4 3 8 15])))
  (is (v/valid? "hello" (v/are char?)))
  (is (not (v/valid? 42 (v/are char?)))))
; TODO: Are and every should provide value chain
; TODO: Field method :field
; TODO: Improve optional

(def i-or-f (v/or is-integer is-float))

(deftest ortest
  (is (v/valid? 3 i-or-f))
  (is (v/valid? 3.14 i-or-f))
  (let [[e] (get-errors i-or-f "foo")
        [e1 e2] (:errors e)]
    (is (= (ValidationError. i-or-f "foo")
          (dissoc e :errors)))
    (is (= e1 (ValidationError. is-integer "foo")))
    (is (= e2 (ValidationError. is-float "foo")))))

(deftest to-val
  (let [v (v/transform keys)]
    (is (= [:a]
           (:result (v/validate v {:a 3}))))))

(def are-string (v/are string?))
(def simple-map-av
  (v/and (av/keys (v/are keyword?)) (av/vals are-string)))
(def simple-map
  (v/and (v/->> keys (v/are keyword?)) (v/->> vals are-string)))

(def up-to-4-elements-av (av/count (v/validator #(< % 4))) )
(def up-to-4-elements (v/->> count (v/validator #(< % 4))) )

(deftest projection-tests
  (is (= [(ValidationError. are-string 2)
          (get-errors simple-map {:a "one", :b 2})]))
  (is (= [(ValidationError. are-string 2)
          (get-errors simple-map-av {:a "one", :b 2})]))
  (is (not (v/valid? [:a :b :c :d] up-to-4-elements)))
  (is (v/valid? [:a :c :d] up-to-4-elements))
  (is (not (v/valid? [:a :b :c :d] up-to-4-elements-av)))
  (is (v/valid? [:a :c :d] up-to-4-elements-av)))

(def john {:name "John Doe", :address {:city "Baltimore"}})

(deftest in
  (testing "Required"
    (is (v/valid? "Hello" v/required))
    (is (v/valid? "Hello" v/optional))
    (is (not (v/valid? nil v/required)))
    (is (not (v/valid? "   " v/required))))
  (testing "Validate compatibility"
    (is (v/valid? john (av/in [:address :city] (v/is string?)))
        "City should be valid.")
    (is (not (v/valid? john (av/in [:address :zip] (v/is string?))))
        "Missing ZIP should be invalid.")
    (is (v/valid? john (av/if-in [:address :zip] (v/is string?)))
        "Missing ZIP should be acceptable with if-in."))
  (testing "Native syntax"
    (is (v/valid? john
                  (v/->> [:address :city] v/required (v/is string?)))
        "City should be valid when required.")
    (is (not (v/valid? john
                       (v/->> [:address :zip]
                              v/required
                              (v/is string?))))
        "Missing ZIP should be required.")
    (is (v/valid? john (v/->> [:address :zip]
                              v/optional
                              (v/is string?)))
        "Missing ZIP should be acceptable when optional.")))

(def dn (v/as v/number))

(def key-projection (v/as :name))
(def key2-projection (v/as [:address :city]))

(deftest transform
  (is (= "John Doe" (:result (v/validate key-projection john))))
  (is (= "Baltimore" (:result (v/validate key2-projection john))))
  (is (= 3 (:result (v/validate dn 3))))
  (is (= "4" (:input (v/validate dn "4"))))
  (is (= 4 (:result (v/validate dn "4"))))
  (is (not (v/valid? "H" dn))))

(deftest hastest
  (is (v/valid? john (av/has [:address :city])))
  (is (not (v/valid? john (av/has [:address :zip])))))


(def comment0 {:email "julian@gmail.com"
               :comment "Short"
               :name ""
               :url "http://xxx/"})

(def bademail {:email "xxx"})

(def email (v/->> :email v/optional (v/is v/email?)))

(deftest email-tests
  (is (not (v/email? "test@abc")))
  (is (v/valid? {:email "xjobcon@phx.com"} email))
  (is (not (v/valid? {:email "test@abc"} email)))
  (is (v/valid? {:email " "} email)))
