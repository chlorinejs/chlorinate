(ns check-server
  (:use [chlorinate.esparse :only [check-esparse]]))

(let [status (check-esparse)]
  (case (:type status)
    :ok true
    :error (do (println (:message status))
               (System/exit 1))))
