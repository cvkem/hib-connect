(defproject hib-connect "0.8.0"
  :description "Small clojure interface to the hibernate ORM."
  :dependencies [
;;[org.clojure/clojure "1.3.0"]
[org.clojure/clojure "1.2.1"]
		 [org.clojure/java.jdbc "0.1.1"]
		 [debugtools "1.0.0"]
                 [org.hsqldb/hsqldb "2.2.4"]
		 [org.hibernate/hibernate-core  "3.5.4-Final"]
		 [javassist/javassist  "3.12.1.GA"]
		 [org.slf4j/slf4j-simple  "1.5.8"]]
  :dev-dependencies [[lein-marginalia "0.6.1"]]	)

