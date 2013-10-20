(ns arianna.runtime
  (:refer-clojure :exclude [and comp cond or when])
  (:require [spyscope.core]
            [poppea :refer [document-partial-% partial-invoke-%
                            defn-curried]]
            [stencil.core :refer [render-string]]))

(def ^:dynamic *enable-protect-exception* true)

(defrecord ValidationError [validator value])

; status is either :ok or :error (:pending may come later)
(defrecord ValidationResult [status result errors input])

(defn- strip-reduced [v] (if (reduced? v) @v v))

(defn report-failure
  ([value] (report-failure nil value value))
  ([this value] (report-failure this value value))
  ([this input result]
     (->ValidationResult :error result
                         (list (ValidationError. this input))
                         input)))

(defn- add-exception [[error] exception]
  (list (assoc error :exception exception)))

(defn internal-validate [{:keys [-method] :as validator} value]
  {:pre (symbol? -method)}
  (if *enable-protect-exception*
    (try ((find-var -method) validator value)
           (catch Exception exception
             (update-in (report-failure validator value)
                        [:errors] add-exception exception)))
    ((find-var -method) validator value)))

(defn validate [validator value]
  (strip-reduced (internal-validate validator value)))

(defn validate-debug [validator value]
  (binding [*enable-protect-exception* false]
    (validate validator value)))

(defn valid?
  "Returns true if value passes the validator."
  ([{:keys [status]}] (= :ok status))
  ([value validator] (valid? (validate validator value))))

(defn report-success
  ([value] (->ValidationResult :ok value nil value))
  ([input result] (->ValidationResult :ok result nil input)))

(defn predicate-validate
  [this result value]
  (if result
    (report-success value)
    (report-failure this value)))

(defn is [this value]
  (predicate-validate this
                      (partial-invoke-% this value)
                      value))

(defn is-optional [this value]
  (let [r (report-success value)]
      (if (partial-invoke-% this value) (reduced r) r)))

(defn is-not [this value]
  (predicate-validate this
                      (not (partial-invoke-% this value))
                      value))



(defn as-key [{:keys [projection default] :as this} value]
  (report-success (if (vector? projection)
                    (get-in value projection default)
                    (get value projection default))))

(defn has [{:keys [projection] :as this} value]
  (let [result (if (vector? projection)
                 (get-in value projection ::missing)
                 (get value projection ::missing))]
    (if (= ::missing result)
      (report-failure this value)
      (report-success value result))))

(defn enhance-error [this result]
  (update-in result [:errors]
             (fn [e] (map #(assoc % :validator this) e))))

(defn as [this value]
  (let [result (partial-invoke-% this value)]
    (if (instance? ValidationResult result)
      (if (valid? result)
        result
        (enhance-error this result))
      (if (= result (:-fail this))
        (report-failure this value result)
        (report-success value result)))))

;;; Validator combinators

(defn merge-errors [previous current]
  (if (valid? previous)
    current
    (update-in current [:errors]
               (partial concat (:errors previous)))))

(defn and-all-f [previous current]
  (if (valid? current)
    previous
    (merge-errors previous current)))

(defn and-f [previous current]
  (if (valid? current)
    current
    (reduced current)))

(defn-curried make-and-combine [f {:keys [validators]} value]
  (->> validators
       (map #(internal-validate % value))
       (reduce f (report-success value))
       strip-reduced))

(def and (make-and-combine and-f))
(def and-all (make-and-combine and-all-f))

(defn or-f [previous current]
  (if (valid? current)
    (reduced current)
    (merge-errors previous current)))

(defn or [{:keys [validators] :as this} value]
  (let [fail (->ValidationResult :error nil [] value)
        vr (->> validators
                (mapv #(internal-validate % value))
                (reduce or-f fail)
                strip-reduced)]
    (if (valid? vr)
      vr
      (let [or-error (assoc (ValidationError. this value)
                       :errors (:errors vr))]
        (->ValidationResult :error
                            value
                            (list or-error)
                            value)))))

;;; threading

(defn apply-reduced [value f]
  (if (reduced? value)
    (reduced (f @value))
    (f value)))

(defn add-chain [validator {:keys [result] :as vr}]
  (update-in vr [:chain] #(conj (clojure.core/or % [])
                                {:validator validator
                                 :result result})))

(defn thread-f [{:keys [result] :as previous} validator]
  (let [vr0 (internal-validate validator result)
        vr (apply-reduced vr0 (partial add-chain validator))]
    (if (clojure.core/or (reduced? vr) (valid? vr))
      vr
      (reduced vr))))

(defn thread [{:keys [validators] :as this} value]
  (->> validators
       (reduce thread-f
               (assoc (report-success value)
                 :chain [{:validator this :result value}]))
       strip-reduced))

;; every
(defn every-validate [validators this input]
  (->> (for [v validators
             item input]
         (validate v item))
       (reduce and-all-f (report-success input))
       strip-reduced))

(defn every [{:keys [validators] :as this} value]
  (every-validate validators this value))

(defn are [this input]
  (let [r (filterv #(not (partial-invoke-% this %)) input)]
    (if (empty? r)
      (report-success this input)
      (->ValidationResult :error
                          input
                          (mapv #(ValidationError. this %) r)
                          input))))

;;; cond and when

(defn always-true [this input] (report-success input))

(defn-curried valid-clause [input clause]
  (valid? input (first clause)))

(defn cond [{:keys [clauses] :as this} input]
  (if-let [clause
           (first (filter (valid-clause input) clauses))]
    (validate (second clause) input)
    (report-failure this input)))

(defn when [{:keys [predicate then]} input]
  (if (valid? (validate predicate input))
    (validate then input)
    (report-success input)))

(defn ^:validator message [this message]
  (assoc this :message message))

(defprotocol ValidationMessage
  (render-message- [this validation-result]))

(extend-protocol ValidationMessage
  String
  (render-message- [this validation-error]
    (render-string this validation-error)))

(defn render-message [validation-error]
  (if-let [message
           (get-in validation-error [:validator :message] nil)]
    (if (satisfies? ValidationMessage message)
      (render-message- message validation-error)
      (message validation-error))))
