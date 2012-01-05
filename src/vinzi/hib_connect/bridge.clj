(ns vinzi.hib-connect.bridge
  (use vinzi.hib-connect.globals)
  (import org.hibernate.SessionFactory)
  (import org.hibernate.cfg.Configuration))


(defmacro br_prl [& args]
;;  (apply println args)
  )
(defmacro br_pr [& args]
;;  (apply println args)
  )

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

(defn force-open-hib []
  (when (.isClosed sessionFactory)
    (def sessionFactory (createSessionFactory))))

;; For the time being I only deliver a function interface
;; use 'call-in-hib-session
;;
;; (defmacro with-hib-session
;;   "Macro that runs the body within a hiberate session. A transaction is opened, next the body is executed and afterwards the transaction is commited."
;;   [& body]
;;   `(do
;;      (let [session (.getCurrentSession sessionFactory)]
;;        (.beginTransaction session)
;;        ~@body
;;        (.. session (getTransaction) (commit)))))

(defn call-in-hib-session [f & args]
  "Calls function 'f' within a hiberate session. A transaction is opened, next the function 'f' is called with arguments 'session' as first argument, followed by 'args' and afterwords the transaction is commited."
  (let [session (.getCurrentSession sessionFactory)]
    (.beginTransaction session)
    (let [res (apply f session args)]
      (.. session (getTransaction) (commit))
    res)))



(defn- transferGenerator
  "The generaric translator (closure). Currently there are two instances,
    ie.  trans-to-Clj and trans-to-java."
  [transGen]
  (fn [rec] (let [translator (transGen rec)]
    (when (nil? translator)
      (br_prl "No translator found for ttem " rec)
      (flush)
      (assert translator))
    (translator rec))))

;; translates a java-object to a clj-instance.
(def trans-to-clj (transferGenerator get-to-clj))

;; translates a clj-instance to a java-object.
(def trans-to-java (transferGenerator get-to-java))


(defn store-hib
  "Function 'store-hib' takes a single clj-record, or a sequence of clj-records
   and store the records one by one (so a sequence might contain a mixture of
   different types.
   This function should be provided a session (via 'call-in-hib-session')"
  [session cRecs]
  (letfn [(store-single [cRec]
			;; translate the record to a java-object and store it
			(let [jRec (trans-to-java cRec)]
			  (br_prl  "saving the record with contents: " jRec)
			  (let [res (.save session jRec)
				newId (.getId jRec)]
			    (if (not= newId (:id cRec))
			      (do
				(br_prl " id changed to: " newId)
				(assoc cRec :id newId))
			      cRec))))]
    (if (or (seq? cRecs) (vector? cRecs))
      (doall (map store-single cRecs))
      (store-single cRecs))))


(defn query-hib
  "Function 'query-hib' runs the query and returns a sequence of clj-objectstakes a single clj-record, or a sequence of clj-records
   and store the records one by one (so a sequence might contain a mixture of
   different types.
   This function should be provided a session (via 'call-in-hib-session')"
[session qryStr]
  (let [qry   (.createQuery session qryStr)
	jRes   (.list qry)
	cRes   (map trans-to-clj jRes)]
    (br_prl "The query: " qryStr)
    (br_prl "returns:")
    (br_prl cRes)
    cRes))

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
(br_prl "about to run query: " (first sql-params))
(br_prl "Next applying local function " func "  containing body")
;;
(let [rset (call-in-hib-session query-hib (first sql-params))]
  (func rset)))


(defn delete-hib
  "This function either takes as argument either an object stored in hibernate
   a (heterogenous) sequence of vector of objects or string that representes
   a hibernate selection. It deletes the object from the hibernate store.
   This function returns the number of deleted items.
   (This operation needs to be run within a 'call-with-hibernate')"
  [session toDel]
  (letfn [(delete-single [cRec]
			;; translate the record to a java-object and store it
			(let [jRec (trans-to-java cRec)]
			  (br_prl  "Deleting the record with contents: " jRec)
			  (.delete session jRec)
			  1))]
    (let [res (if (= (type toDel) (type ""))
		(let [res (.delete session toDel)]
		  (br_prl "Deleting records for select-string: " toDel)
		  res)
		(if (or (seq? toDel) (vector? toDel))
		  (count (doall (map delete-single toDel)))
		  (delete-single toDel)))]
      (br_prl "Deleted  " res " items")
      res)))


