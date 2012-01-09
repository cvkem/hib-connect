(ns vinzi.hib-connect.bridge
  (use vinzi.hib-connect.globals)
  (import org.hibernate.SessionFactory)
  (import org.hibernate.cfg.Configuration))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Functions for managing the connections
;;

(defn createSessionFactory "Create a sessionFactory" []
  (try
    (.. (Configuration.) (configure) (buildSessionFactory))
    (catch Throwable except
      (println "Exception: " (.getMessage except)))))

;; a global variable is used to store the sessionFactory.
;; This sessionFActory is used by the call-with-hibernate-session.
(defonce sessionFactory (createSessionFactory))


(defn close-hib "close the session-factory (shutdown)" []
  (.close sessionFactory))

(defn force-open-hib
  "Check the status of the current session. If it is closed reopen it."
  []
  (when (.isClosed sessionFactory)
    (def sessionFactory (createSessionFactory))))


(defn exec-hib-transaction [f & args]
  "Calls function 'f' within a hiberate session. A transaction is opened, next the function 'f' is called with arguments 'session' as first argument, followed by 'args' and afterwords the transaction is commited."
  (let [session (.getCurrentSession sessionFactory)]
    (.beginTransaction session)
    (let [res (apply f session args)]
      (.. session (getTransaction) (commit))
    res)))

;; For the time being I only deliver a function interface
;; use 'exec-hib-transaction
;;
;; (defmacro with-hib-session
;;   "Macro that runs the body within a hiberate session. A transaction is opened, next the body is executed and afterwards the transaction is commited."
;;   [& body]
;;   `(do
;;      (let [session (.getCurrentSession sessionFactory)]
;;        (.beginTransaction session)
;;        ~@body
;;        (.. session (getTransaction) (commit)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Functions for translation between the clojure and java domain.
;;


;; translates a java-object to a clj-instance.
(def trans-to-clj (transferGenerator get-to-clj))

;; translates a clj-instance to a java-object.
(def trans-to-java (transferGenerator get-to-java))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Functions for datamanagement
;;



(defn store-hib*
  "Function 'store-hib' takes a single clj-record, or a sequence of clj-records
   and store the records one by one (so a sequence might be heterogeneous
   (contain a mixture of different types).
   Internal functions to be called with a 'exec-hib-transaction'."
  [session cRecs]
  (letfn [(store-single [cRec]
			;; translate the record to a java-object and store it
			(let [jRec (trans-to-java cRec)]
			  (hib_prl  "saving the record with contents: " jRec)
			  (let [res (.save session jRec)
				newId (.getId jRec)]
			    (if (not= newId (:id cRec))
			      (do
				(hib_prl " id changed to: " newId)
				(assoc cRec :id newId))
			      cRec))))]
    (if (or (seq? cRecs) (vector? cRecs))
      (doall (map store-single cRecs))
      (store-single cRecs))))

(defn store-hib
  "Function 'store-hib' takes a single clj-record, or a sequence of clj-records
   and store the records one by one (so a sequence might be heterogeneous
   (contain a mixture of different types)."
  [cRecs]
  (exec-hib-transaction store-hib* cRecs))


(defn query-hib*
  "Function 'query-hib' runs the query and returns a sequence of clj-objectstakes a single clj-record, or a sequence of clj-records
   and store the records one by one (so a sequence might contain a mixture of
   different types.
   This function should be provided a session (via 'exec-hib-transaction')"
[session qryStr]
  (let [qry   (.createQuery session qryStr)
	jRes   (seq (.list qry))
	cRes   (map trans-to-clj jRes)]
    (hib_prl "The query: " qryStr)
    (hib_prl "returns:")
    (hib_prl cRes)
    cRes))

(defn query-hib
  "Function 'query-hib' runs the query and returns a sequence of clj-objectstakes a single clj-record, or a sequence of clj-records
   and store the records one by one (so a sequence might contain a mixture of
   different types."
  [qryStr]
  (exec-hib-transaction query-hib* qryStr))

(defmacro with-query-results
  "Executes a query, then evaluates body with results bound to a seq of the
   results. sql-params is a vector containing a string providing
   the (optionally parameterized) SQL query followed by values for any
   parameters.
   (code taken from 'clojure.sql')"
[results sql-params & body]
`(with-query-results* ~sql-params (fn [~results] ~@body)))

(defn with-query-results*
"Executes a query, then evaluates func passing in a seq of the results as
an argument. The first argument is a vector containing the (optionally
parameterized) sql query string followed by values for any parameters.
(partially based on code taken from 'clojure.sql')"
[[sql & params :as sql-params] func]
(when-not (vector? sql-params)
  (println "expected a vector for sql-params"))
(when (not= (count sql-params) 1)
  (println "The current implementation will only handle the first sql-param")
  (println "Discarding: " (rest sql-params)))
;;
(hib_prl "about to run query: " (first sql-params))
(hib_prl "Next applying local function " func "  containing body")
;;
(let [rset (query-hib (first sql-params))]
  (func rset)))


(defn delete-hib*
  "This function either takes as argument either an object stored in hibernate
   a (heterogenous) sequence of vector of objects or string that representes
   a hibernate selection. It deletes the object from the hibernate store.
   This function returns the number of deleted items.
   (This operation needs to be run within a 'call-with-hibernate')"
  [session toDel]
  (letfn [(delete-single [cRec]
			;; translate the record to a java-object and store it
			(let [jRec (trans-to-java cRec)]
			  (hib_prl  "Deleting the record with contents: " jRec)
			  (.delete session jRec)
			  1))]
    (let [res (if (= (type toDel) (type ""))
		(let [res (.delete session toDel)]
		  (hib_prl "Deleting records for select-string: " toDel)
		  res)
		(if (or (seq? toDel) (vector? toDel))
		  (count (doall (map delete-single toDel)))
		  (delete-single toDel)))]
      (hib_prl "Deleted  " res " items")
      res)))

(defn delete-hib
  "This function either takes as argument either an object stored in hibernate
   a (heterogenous) sequence of vector of objects or string that representes
   a hibernate selection. It deletes the object from the hibernate store.
   This function returns the number of deleted items."
  [toDel]
  (exec-hib-transaction delete-hib* toDel))


