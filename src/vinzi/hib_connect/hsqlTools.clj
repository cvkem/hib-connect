(ns vinzi.hib-connect.hsqlTools
  (:require [clojure.java  [jdbc :as sql]]))



;; default settings using an in-memory hypersonic database
(def db {
	 :classname "org.hsqldb.jdbc.JDBCDriver"
	 :subprotocol "hsqldb"
	 :subname  "hsql://localhost:9001/testdb"
	 :user  "SA"
	 :password ""})



(defn doSql
  "Execute an Sql statement for it's side effects on the database.
   If you want to view the output then use 'showSql'."
  [& cmds]
  (println "Use ShowSql if you want to view the output")
  (sql/with-connection db (apply sql/do-commands cmds)))

(defn showSql [& cmds]
  (letfn [(doShow [cmd]
		   (println "running command: " cmd)
		   (sql/with-query-results res
		     [(str cmd)]
		     (doseq [rec res]
		       (println rec))))
	   ]
  (sql/with-connection db (doseq [cmd cmds] (doShow cmd)))))

(defmacro doComm [cmd]
  `(sql/with-connection db ~cmd)) 

;;(showSql "select * from int_rec")
