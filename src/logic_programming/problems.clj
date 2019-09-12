(ns logic-programming.problems
  (:require [clojure.core.logic :as logic]
            [sicp-logic.core :as sicp]
            [sicp-logic.db.memory :as memdb]))

(defmacro with-facts [db facts & body]
  `(do
     (doseq [fact# ~facts]
       (sicp/assert! ~db fact#))
     ~@body))

;; PROBLEM 1
;; Given a database of users and tweets, write a logic program that finds
;; the tweets of all the users that a particular user follows

(defn problem-1 [user]
  (let [db (memdb/new-db)]
    (with-facts db '[[:tweet user1 "Hello, world!"]
                     [:tweet user1 "Logic is fun!"]
                     [:tweet user2 "Foo bar baz"]
                     [:tweet user2 "Qux"]
                     [:tweet user3 "The quick brown fox"]
                     [:tweet user3 "Jumped over the lazy dog"]
                     [:follows user1 user2]
                     [:follows user1 user3]
                     [:follows user2 user1]
                     [:follows user3 user1]
                     [:follows user3 user2]]
      (sicp/query* db ['and [:follows user '?other-user]
                            [:tweet '?other-user '?tweet]]))))

;; PROBLEM 2
;; Given a database of users, write a logic program that computes the relation
;; (common-follows p1 p2 l) which should be true if l is the list of the users
;; that p1 and p2 both follow
(defn problem-2 [p1 p2 l]
  (let [db (memdb/new-db)]
    (with-facts db '[[follows user1 user2]
                     [follows user1 user3]
                     [follows user2 user1]
                     [follows user3 user1]
                     [follows user3 user2]
                     [follows user1 user4]
                     [follows user4 user2]
                     [follows user2 user4]]
      (sicp/defrule! db [common-follows ?p1 ?p2 [?x]]
        (and [follows ?p1 ?x]
             [follows ?p2 ?x]))
      (sicp/defrule! db [common-follows ?p1 ?p2 [?x & ?xs]]
        (and [follows ?p1 ?x]
             [follows ?p2 ?x]
             [common-follows ?p1 ?p2 ?xs]))
      (sicp/query* db ['common-follows p1 p2 l]))))
        

;; PROBLEM 3
;; Write a logic program that computes the relation (append-to-form x y z),
;; which should be true if x and y append to form z.

(defn problem-3 [x y z]
  (let [db (memdb/new-db)]
    (sicp/defrule! db [append-to-form [] ?y ?y])
    (sicp/defrule! db [append-to-form [?u & ?v] ?y [?u & ?z]]
      (append-to-form ?v ?y ?z))
    (sicp/query* db ['append-to-form x y z])))
