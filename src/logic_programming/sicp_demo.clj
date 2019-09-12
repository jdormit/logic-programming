(ns logic-programming.sicp-demo
  (:require [sicp-logic.core :refer :all]
            [sicp-logic.db.memory :refer [new-db]]))

;; Facts are arbitrary sequences
(def facts '[(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
             (job (Bitdiddle Ben) (computer wizard))
             (salary (Bitdiddle Ben) 60000)
             (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
             (job (Hacker Alyssa P) (computer programmer))
             (salary (Hacker Alyssa P) 40000)
             (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
             (address (Fect Cy D) (Cambridge (Ames Street) 3))
             (job (Fect Cy D) (computer programmer))
             (salary (Fect Cy D) 35000)
             (supervisor (Fect Cy D) (Bitdiddle Ben))
             (address (Tweakit Lem E) (Boston (Bay State Road) 22))
             (job (Tweakit Lem E) (computer technician))
             (salary (Tweakit Lem E) 25000)
             (supervisor (Tweakit Lem E) (Bitdiddle Ben))
             (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
             (job (Reasoner Louis) (computer programmer trainee))
             (salary (Reasoner Louis) 30000)
             (supervisor (Reasoner Louis) (Hacker Alyssa P))
             (supervisor (Bitdiddle Ben) (Warbucks Oliver))
             (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
             (job (Warbucks Oliver) (administration big wheel))
             (salary (Warbucks Oliver) 150000)
             (address (Scrooge Eben) (Weston (Shady Lane) 10))
             (job (Scrooge Eben) (accounting chief accountant))
             (salary (Scrooge Eben) 75000)
             (supervisor (Scrooge Eben) (Warbucks Oliver))
             (address (Cratchet Robert) (Allston (N Harvard Street) 16))
             (job (Cratchet Robert) (accounting scrivener))
             (salary (Cratchet Robert) 18000)
             (supervisor (Cratchet Robert) (Scrooge Eben))
             (address (Aull DeWitt) (Slumerville (Onion Square) 5))
             (job (Aull DeWitt) (administration secretary))
             (salary (Aull DeWitt) 25000)
             (supervisor (Aull DeWitt) (Warbucks Oliver))
             (can-do-job (computer wizard) (computer programmer))
             (can-do-job (computer wizard) (computer technician))
             (can-do-job (computer programmer) (computer programmer trainee))
             (can-do-job (administration secretary) (administration big wheel))])

(defmacro defexample [name [db-var & args] & body]
  `(defn ~name [~@args]
     (let [~db-var (new-db)]
       (doseq [fact# facts]
         (assert! ~db-var fact#))
       ~@body)))

;; The simplest query pattern matches against facts in the database
(defexample simple-query [db]
  (query db (job ?x (computer programmer))))

;; Querying returns the query with all possible variable bindings filled in
;; ((job (Hacker Alyssa P) (computer programmer))
;;  (job (Fect Cy D) (computer programmer))

;; Patterns can have more than one variable
(defexample multiple-variables [db]
  (query db (address ?x ?y)))

;; ((address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
;;  (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
;;  (address (Fect Cy D) (Cambridge (Ames Street) 3))
;;  (address (Tweakit Lem E) (Boston (Bay State Road) 22))
;;  (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
;;  (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
;;  (address (Scrooge Eben) (Weston (Shady Lane) 10))
;;  (address (Cratchet Robert) (Allston (N Harvard Street) 16))
;;  (address (Aull DeWitt) (Slumerville (Onion Square) 5)))

;; Pattern matching works in sub-sequences too
(defexample sub-sequence-matching [db]
  (query db (job ?x (computer ?type))))

;; ((job (Bitdiddle Ben) (computer wizard))
;;  (job (Hacker Alyssa P) (computer programmer))
;;  (job (Fect Cy D) (computer programmer))
;;  (job (Tweakit Lem E) (computer technician)))

;; The & symbol matches the first and rest elements of a sequence
;; as in core.match or destructuring
(defexample &-matching [db]
  (query db (job ?x (computer & ?type))))

;; ((job (Bitdiddle Ben) (computer & (wizard)))
;;  (job (Hacker Alyssa P) (computer & (programmer)))
;;  (job (Fect Cy D) (computer & (programmer)))
;;  (job (Tweakit Lem E) (computer & (technician)))
;;  (job (Reasoner Louis) (computer & (programmer trainee))))

;; (and) and (or) do what you'd expect
(defexample and-query [db]
  (query db (and (job ?person (computer programmer))
                 (address ?person ?where))))

;; ((and
;;   (job (Hacker Alyssa P) (computer programmer))
;;   (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
;;  (and
;;   (job (Fect Cy D) (computer programmer))
;;   (address (Fect Cy D) (Cambridge (Ames Street) 3))))
            

(defexample or-query [db]
  (query db (or (supervisor ?x (Bitdiddle Ben))
                (supervisor ?x (Hacker Alyssa P)))))

;; ((or
;;   (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
;;   (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
;;  (or
;;   (supervisor (Fect Cy D) (Bitdiddle Ben))
;;   (supervisor (Fect Cy D) (Hacker Alyssa P)))
;;  (or
;;   (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;;   (supervisor (Tweakit Lem E) (Hacker Alyssa P)))
;;  (or
;;   (supervisor (Reasoner Louis) (Bitdiddle Ben))
;;   (supervisor (Reasoner Louis) (Hacker Alyssa P))))

;; (not) and (lisp-value) act as filters on the set of valid conclusions.
;; This means that they typically come as a subsequent clause in an (and)
(defexample not-query [db]
  (query db (and (supervisor ?x (Bitdiddle Ben))
                 (not (job ?x (computer programmer))))))

;; ((and
;;   (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;;   (not (job (Tweakit Lem E) (computer programmer)))))

(defexample lisp-value-query [db]
  (query db (and (salary ?person ?amount)
                 (lisp-value > ?amount 30000))))

;; ((and (salary (Bitdiddle Ben) 60000) (lisp-value > 60000 30000))
;;  (and (salary (Hacker Alyssa P) 40000) (lisp-value > 40000 30000))
;;  (and (salary (Fect Cy D) 35000) (lisp-value > 35000 30000))
;;  (and (salary (Warbucks Oliver) 150000) (lisp-value > 150000 30000))
;;  (and (salary (Scrooge Eben) 75000) (lisp-value > 75000 30000)))

;; Note that since (not) and (lisp-value) act as a filter, putting one of them
;; first in a query will give unexpected results
(defexample bad-not-query [db]
  (query db (and (not (job ?x (computer programmer)))
                 (supervisor ?x (Bitdiddle Ben)))))

;; ()

;; Rules are the means of abstraction in the language. Rules consist of a
;; conclusion and a body, where the conclusion holds if the body is true.
;; If there is no body, the rule holds for any fact that matches its conclusion.

(defexample simple-rule [db]
  (defrule! db (same ?x ?x))
  (defrule! db (lives-near ?person-1 ?person-2)
    (and (address ?person-1 (?town & ?rest-1))
         (address ?person-2 (?town & ?rest-2))
         (not (same ?person-1 ?person-2))))
  (query db (lives-near ?x (Bitdiddle Ben))))

;; ((lives-near (Reasoner Louis) (Bitdiddle Ben))
;;  (lives-near (Aull DeWitt) (Bitdiddle Ben)))

;; Rules can be recursive
(defexample recursive-rule [db]
  (defrule! db (outranked-by ?staff-person ?boss)
    (or (supervisor ?staff-person ?boss)
        (and (supervisor ?staff-person ?middle-manager)
             (outranked-by ?middle-manager ?boss))))
  (query db (outranked-by (Tweakit Lem E) ?who)))

;; ((outranked-by (Tweakit Lem E) (Bitdiddle Ben))
;;  (outranked-by (Tweakit Lem E) (Warbucks Oliver)))

;; Queries don't need to involve facts in the database to be interesting.
;; This query can make deductions about the relation (append-to-form x y z)
;; which holds true if x and y append to form z.

(defexample append-rule [db]
  (defrule! db (append-to-form () ?y ?y))
  (defrule! db (append-to-form (?u & ?v) ?y (?u & ?z))
    (append-to-form ?v ?y ?z))
  (query db (append-to-form ?x ?y (a b c d))))

;; ((append-to-form () (a b c d) (a b c d))
;;  (append-to-form (a & ()) (b c d) (a b c d))
;;  (append-to-form (a & (b & ())) (c d) (a b c d))
;;  (append-to-form (a & (b & (c & ()))) (d) (a b c d))
;;  (append-to-form (a & (b & (c & (d & ())))) () (a b c d)))
