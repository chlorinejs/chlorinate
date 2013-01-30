(ns chlorinate.core
  (:use [chlorinate.esparse]
        [chlorinate.to-cl2 :only [to-cl2]])
  (:require [clojure.pprint]))

(defn js->cl2 [s]
  (vec (map to-cl2 (esparse s))))

(defn format-code [form]
  (clojure.pprint/write form :pretty true :stream nil))
