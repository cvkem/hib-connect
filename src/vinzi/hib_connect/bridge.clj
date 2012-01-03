(ns vinzi.hib-connect.bridge
  (use vinzi.hib-connect.globals)
  (import org.hibernate.SessionFactory)
  (import org.hibernate.cfg.Configuration))


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


(defmacro with-hib-session
  "Macro that runs the body within a hiberate session. A transaction is opened, next the body is executed and afterwards the transaction is commited."
  [& body]
  `(do
     (let [session (.getCurrentSession sessionFactory)]
       (.beginTransaction session)
       ~@body
       (.. session (getTransaction) (commit)))))

(defn call-in-hib-session [f & args]
  "Calls function 'f' within a hiberate session. A transaction is opened, next the function 'f' is called with arguments 'session' as first argument, followed by 'args' and afterwords the transaction is commited."
  (let [session (.getCurrentSession sessionFactory)]
    (.beginTransaction session)
    (apply f session args)
    (.. session (getTransaction) (commit))))



(defn transferGenerator
  "The generaric translator (closure). Currently there are two instances,
    ie.  trans-to-Clj and trans-to-java."
  [transGen]
  (fn [rec] (let [translator (transGen rec)]
    (when (nil? translator)
      (println "No translator found for ttem " rec)
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
			  (println  "saving the record with contents: " jRec)
			  (.save session jRec)))]
    (if (seq? cRecs)
      (doseq [cRec cRecs] (store-single cRec))
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
    (println "The query: " qryStr)
    (println "returns:")
    (println cRes)
    cRes))


;; package vinzi.cdfdeMgt.util;

;; import org.hibernate.SessionFactory;
;; import org.hibernate.cfg.Configuration;

;; public class HibernateUtil {

;;     private static final SessionFactory sessionFactory = buildSessionFactory();
;;     private static SessionFactory buildSessionFactory() {
;; 	try {
;; 	    // Create the SessionFactory from hibernate.cfg.xml
;; 	    return new Configuration().configure().buildSessionFactory();
;; 	}
;; 	catch (Throwable ex) {
;; 	    // Make sure you log the exception, as it might be swallowed
;; 	    System.err.println("Initial SessionFactory creation failed." + ex);
;; 	    throw new ExceptionInInitializerError(ex);
;; 	}
;;     }
;;     public static SessionFactory getSessionFactory() {
;; 	return sessionFactory;
;;     }
;; }
