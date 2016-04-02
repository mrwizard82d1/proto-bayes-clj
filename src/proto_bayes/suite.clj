(ns proto-bayes.suite)

(defn make-suite [hypotheses]
  "Make a suite. A suite is set of hypotheses (and, eventually, associated probabilites) that are 
  exhaustive and mutually exclusive. (See the book \"Think Bayes\" for additional details."
  {:hypotheses hypotheses :prior (zipmap hypotheses (repeat 1))})

(defn hypotheses [suite]
  "Returns the hypotheses for suite."
  (:hypotheses suite))

(defn add-prior [suite prior]
  "Add prior (a function of a single argument, a hypothesis). Remember that a map between
   hypotheses and probabilities qualifies."
  (assoc suite :prior prior))

(defn add-likelihood [suite likelihood]
  "Add a likelihood function of two arguments, data and hypothesis, that calculates the likelihood
   of data given a hypothesis is true."
  (assoc suite :likelihood likelihood))

(defn prior [suite hypothesis]
  "Calculate the prior probability of hypothesis in suite."
  ((:prior suite) hypothesis))

(def probability prior)

(defn likelihood [suite data hypothesis]
  "Calculate the likelihood of data given hypothesis in suite."
  ((:likelihood suite) data hypothesis))

(defn posterior [suite data]
  "Calculates the posterior probabilities of seeing data in a suite."
  (let [unnormalized-posteriors (map #(* (prior suite %1) (likelihood suite :vanilla %1))
                                     (hypotheses suite))
        total-probability (reduce + unnormalized-posteriors)
        normalized-posteriors (map #(/ % total-probability) unnormalized-posteriors)]
    (-> (hypotheses suite)
        (make-suite)
        (add-prior (zipmap (hypotheses suite) normalized-posteriors))
        (add-likelihood (:likelihood suite)))))
