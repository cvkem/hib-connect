(ns vinzi.hib-connect.bridge
  (use vinzi.hib-connect.globals)
  (require [clojure.string :as str])
  (import org.hibernate.SessionFactory)
  (import org.hibernate.cfg.Configuration))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Functions for managing the connections
;;

;; if dbParam is nill the hibernate will be initialized from the
;; hibernate.settings.xml. If db is set this will be used to initialize.
;;switched back to 1.2
;;(def ^:dynamic dbParams nil)
(def dbParams nil)

;; a global variable is used to store the sessionFactory.
;; This sessionFActory is used by the call-with-hibernate-session.
;;(def ^:dynamic sessionFactory nil)
(def  sessionFactory nil)



(def csfPropMap {:classname "connection.driver_class"
		 :url "connection.url"
		 :user "connection.username"
		 :dialect "dialect"
		 :password "connection.password"
		 :pool-size "connection.pool_size"
		 :current_session_context_class "current_session_context_class"
		 :cache-provider-class "cache.provider_class"
		 :show-sql  "show_sql"
		 :hbm2ddl-auto "hbm2dll.auto"})

;; default values for all properties
(def csfPropDef {:classname "org.hsqldb.jdbcDriver"
		 :url "jdbc:hsqldb:hsql://localhost/testdb"
		 :dialect "org.hibernate.dialect.HSQLDialect"
		 :user "user"
		 :password ""
		 :pool-size "1"
		 :current_session_context_class "thread"
		 :cache-provider-class "org.hibernate.cache.NoCacheProvider"
		 :show-sql  "true"
		 :hbm2ddl-auto "update"})


;; mapping from subprotocol to dialect
;; Note: For mySql you also have other options
;; - MySQL5InnoDBDialect 	 
;; - MySQLDialect 	An SQL dialect for MySQL (prior to 5.x).
;; - MySQLInnoDBDialect 	 
;; - MySQLMyISAMDialect
;; For  Postgress you also have option 'PostgresPlusDialect'
;; For hypersonic you also have option
;;     'HSQLDialect.ReadUncommittedLockingStrategy'
(def csfDialectMap {"mysql"          "MySQL5Dialect" 
		    "hsqldb:hsql"    "HSQLDialect"
		    "postgresql"     "PostgreSQLDialect"})

(defn set-db-params "Set the dbParam record based on the baseProperties that are passed"
  [baseProps]
  (letfn [(check-dialect [db]
			 ;; try to derive the dialect from the subprotocol
			 ;; if it is not set
			 (if (:dialect db)
			   db
			   (let [subprotocol (:subprotocol db)
				 dialect (get csfDialectMap subprotocol)
				 dialect (when dialect (str
					     "org.hibernate.dialect." dialect))]
			     (if dialect (assoc db :dialect dialect) db))))
	  (derive-url [db]
		      ;; derive the url from db-host, subprotocol,
		      ;; db-port and db-name and replace by the url
		      ;; (example jdbc:hsqldb:hsql://localhost:9001/testdb)
		      (let [{:keys [db-host subprotocol db-port db-name]} db
			    db (dissoc db :db-host :subprotocol
				       :db-port :db-name)
			    db-port (when db-port (str ":" db-port))
			    url (str "jdbc:" subprotocol
				     "://" db-host db-port
				     "/" db-name)]
			(assoc db :url url)))
	  (add-defaults [db]
			(into csfPropDef db))]
    (let [;; extract the list of resources (white-space separated)
	  resources (:resources baseProps)
	  resources (when resources(-> resources
				       (str/trim)
				       (str/split #"\s+")))
	  ;; pre-process the db-params record
	  db (-> baseProps
		 (dissoc :resources) 
		 (check-dialect)
		 (derive-url)
		 (add-defaults))]
      ;; set the (dynamic) var dbParam with the obtained result
      (def dbParams {:db        db
		     :resources resources}))))
    
(defn props-createSessionFactory "Create a sessionFactory on the properties mentioned in the dbParam record." [{:keys [db resources]}]
  (letfn [(set-property [csf kv]
			;; set property given by the key-value pair to 'csf'
			(let [k (key kv)
			      v (val kv)
			      p (get csfPropMap k)]
			  (println " for key '" k "' set property '" p
				   "' to value: " v)
			  (if p
			    (let [ret (.setProperty csf p v)]
			      (println "  returning: " ret)(flush)
			      ret)
			    (do
			      (println "WARNING: Property: " k
				       "with value: " v
				       "is not recognized")
			      (println "   ... option ignored")(flush)
			      csf))))
	  (add-resource [csf res]
			;; add a resource to the config 'csf'
			(let [ret (.addResource csf res)]
			  (println "resource " res " returned: "ret) (flush)
			  ret))]
    (println "Setting up a session factory with")
    (println "   Params: " db)
    (println "   Resources: " resources) (flush)
    (try
      ;; build a configuration
      (let [csf (Configuration.)
	    ;; process dialect before connection properties
	    dialect (:dialect db)
	    db   (dissoc db :dialect)
	    csf (if dialect (.setProperty csf "dialect" dialect) csf)
	    csf (reduce set-property csf db)
	    csf (reduce add-resource csf resources)]
	;; and build and return the SessionFactory
	(println "csf = " csf)
	(.buildSessionFactory csf))
      (catch Throwable except
	(println "Exception: " (.getMessage except))))))
;;     (.. (Configuration.)
;; 	;;    <!-- SQL dialect -->
;; 	(setProperty "dialect" 	"org.hibernate.dialect.HSQLDialect")
;; 	;;
;; 	(setProperty "connection.driver_class" 	)
;; 	(setProperty "connection.url" 	)
;; 	(setProperty "connection.username" 	"sa")
;; 	(setProperty "connection.password" 	"")
;; 	;;    <!-- JDBC connection pool (use the built-in) -->
;; 	(setProperty "connection.pool_size" 	"1")
;; 	;;    <!-- Enable Hibernate's automatic session context management -->
;; 	(setProperty "current_session_context_class" 	"thread")
;; 	;;    <!-- Disable the second-level cache -->
;; 	(setProperty "cache.provider_class" 	)
;; 	;;    <!-- Echo all executed SQL to stdout -->
;; 	(setProperty "show_sql" 	)
;; 	;;   <!-- Drop and re-create the database schema on startup -->
;; 	(setProperty "hbm2ddl.auto" 	"update")
;; ;;
;; 	(buildSessionFactory))
;;     (catch Throwable except
;;       (println "Exception: " (.getMessage except)))))

(defn xml-createSessionFactory "Create a sessionFactory based on the hibernate.settings.xml files at the class-path." []
  (println "Setting up a hibernate session-factory from xml")
  (try
    (.. (Configuration.) (configure) (buildSessionFactory))
    (catch Throwable except
      (println "Exception: " (.getMessage except)))))

(defn createSessionFactory "Create a sessionFactory. Use 'dbParams' if available, otherwise look for an xml file." []
  (if dbParams
    (props-createSessionFactory dbParams)
    (xml-createSessionFactory))) 



;;(def ^:dynamic sessionFactory nil)


(defn close-hib "close the session-factory (shutdown)" []
  (.close sessionFactory))

(defn force-open-hib
  "Check the status of the current session. If it is closed reopen it."
  []
  (println "Force-open-hib:")
  (println "Currently using sessionFactory" sessionFactory)
  ;;(when sessionFactory
    ;;(println "  status " (.isClosed sessionFactory)))
  (when (or (nil? sessionFactory)
	    (.isClosed sessionFactory))
    (def sessionFactory (createSessionFactory))
    (println "generated sessionFactory: " sessionFactory)))


(defn exec-hib-transaction [f & args]
  "Calls function 'f' within a hiberate session. A transaction is opened, next the function 'f' is called with arguments 'session' as first argument, followed by 'args' and afterwords the transaction is commited."
  (force-open-hib)
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


