(ns chlorinate.esparse
  (:use [cheshire.core :only [parse-string generate-string]]
        [clojure.java.shell :only [sh]])
  (:require [clojure.string :as s]))

(defn esparse
  "Parses a string of javascript code with esparse-cl2,
return a vector of s-expressions."
  ([s]
     (:body (parse-string (:out (sh "esparse-cl2" :in s))
                          true))))

(defn esgenerate
  "Generates javascript code from an s-expression with escodegen-cl2"
  [m]
  (s/trim (:out (sh "escodegen-cl2" :in (generate-string m)))))

(defn check-esparse []
  (try (if (= (esparse "x;")
              [{:type "ExpressionStatement",
                :expression {:type "Identifier", :name "x"}}])
         {:type :ok}
         {:type :error
          :message (str "ERROR: Something wrong happened."
                        "Esparse-cl2 didn't work properly.")})
       (catch java.io.IOException e
         {:type :error
          :message (str "ERROR: esparse-cl2 command not found!"
                        "Please install it first!")})))
