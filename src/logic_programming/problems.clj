(ns logic-programming.problems
  (:require [clojure.core.logic :as logic]
            [sicp-logic.core :as sicp]
            [sicp-logic.db.memory :as memdb]))

;; PROBLEM 3
;; Write a logic program that computes the relation (append-to-form x y z),
;; which should be true if x and y append to form z.

(defn problem-3 []
  (let [db (memdb/new-db)]
    (sicp/defrule! db [append-to-form [] ?y ?y])
    (sicp/defrule! db [append-to-form [?u & ?v] ?y [?u & ?z]]
      (append-to-form ?v ?y ?z))
    (sicp/query db [append-to-form [a b] [c d] ?z])))
