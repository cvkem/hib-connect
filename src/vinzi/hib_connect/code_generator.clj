(ns vinzi.hib-connect.code-generator
  (require [clojure.string :as str])
  (import  [java.io File]))

(def tablePrefix (atom ""))

(defn set-table-prefix [prefix]
  (swap! tablePrefix (fn [_] prefix)))

(defn write-src-file
  "Write a file with the provided contents."
  [fileName contents]
  (let [file (File. fileName)]
    (print " file '" fileName "'")
    (if (.exists file)
      (println "  exists already and will be overwritten")
      (println "  is a new file"))
    (spit file contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to generate a java source-file that defines the
;; class (objects) that will be stored in hibernate)
;;

(def headerFmt "package %s;\n
public class %s {")
(def fieldFmt "
    public %s %s;
    public %s get%s() { return %s;}
    %s void set%s(%s %s)  {this.%s = %s; };\n\n")
(def constructFmt "
    public %s() {};\n
    public %s(%s) {\n%s\treturn;};\n\n")
(def toStringFmt "   public String toString() {
        return %s;};\n\n")
(def footFmt "};\n")


(defn getSet-postfix
  "Getters and setters need a fieldname where the first character
   is in upper-case."
  [name]
  (apply str (str/upper-case (first name)) (rest name)))

(defn java-field-def
  "Produce a string with a type-definition, a getter and a setter."
  [[fName fType fSetAccess]]
  (let [gsName (getSet-postfix fName)
	fSetAccess  (if fSetAccess fSetAccess "public")]
    (format fieldFmt fType fName
	    fType gsName fName
	    fSetAccess gsName fType fName fName fName)))

(defn java-constructor-def
  "Define a default constructor and a constructor that takes all parameters."
  [name fields]
  (let [args (map #(str (second %) " " (first %)) fields)
	args (apply str (interpose ", " args))
	assign (map #(format "\tthis.%s = %s;\n" %1 %1) (map first fields))
	assign (apply str assign)]
    (format constructFmt name name args assign)))

(defn java-toString
  "Define a toString function that prints the fields prefixed by its name"
  [fields]
  (let [fields (map first fields)
	toString  (map #(format "\" %s=\" + this.%s" %1 %1) fields)
	toString (apply str (interpose " + " toString))]
    (format toStringFmt toString)))

(defn gen-java [package name fields]
  (let [head    (format headerFmt package name)
	fldStr  (map java-field-def fields)
	fldStr  (apply str fldStr)
	cstrStr (java-constructor-def name fields)
	toStr   (java-toString fields)
	fileName `("src" "java" ~(str name ".java"))
	fileName (apply str (interpose File/separator fileName))]
    (write-src-file fileName (str head fldStr cstrStr toStr footFmt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to generate a clojure source-file that defines the
;; records (hash-maps) used in clojure and the translators to
;; between clojure-hasmaps and java-objects.


(def cljSuffix "_clj")

(def cljHeadFmt  "(ns %s.clj.%s
    (:use vinzi.hib-connect.globals)
    (:import %s.%s))\n\n")
(def defrecordFmt "(defrecord %s [%s])\n\n")
(def cljToJavaFmt "(defn %s-to-java [cRec]\n\t(%s. %s))\n\n
(add-to-java (%s. %s) %s-to-java)\n\n")
(def cljCreateFmt "(defn create-%s [%s]
  ;; TO DO:  insert type-checking (java-objects are strictly typed)
  (%s. nil %s))\n\n")
(def cljCloneFmt "(defn clone-%s [cRec]
  ;; TO DO:  insert type-checking cRec should be same type/class as target
  (create-%s %s))\n\n")
(def javaToCljFmt "(defn %s-to-clj [jRec]\n\t(%s. %s))\n\n
(add-to-clj (%s.) %s-to-clj)\n\n")

(defn clj-head [package name]
     (format cljHeadFmt package name package name))

(defn clj-defrecord
  "Define the defrecord for the clj-side of the datastructure."
  [name fNames]
  (let [flds (apply str (interpose " " fNames))]
    (format defrecordFmt name flds)))

(defn clj-to-java
  "Define a translator that maps a clojure defrecord to a java class."
  [name fNames cljName]
  (let [fields (map #(format "(:%s cRec) " %) fNames)
	fields (apply str fields)
	args   (apply str (interpose " " (range (count fNames))))]
    (format cljToJavaFmt name name fields
	    cljName args name)))

(defn skip-id
  "Skip the first item of the sequence if it is an 'id' item"
  [fields matchFunc]
  (if (= (matchFunc fields) "id") (rest fields) fields))

(defn clj-create [name fNames cljName]
  (let [fNames (skip-id fNames first)
;;	fNames (if (= (first fNames) "id") (rest fNames) fNames)
	fNames (apply str (interpose " " fNames))]
    (format cljCreateFmt name fNames
	    cljName fNames)))

(defn clj-clone [name fNames]
  (let [fNames (skip-id fNames first)
	fNames (map #(format "(:%s cRec)" %) fNames)
	fNames (apply str (interpose " " fNames))]
    (format cljCloneFmt name
	    name fNames)))

(defn java-to-clj
  "Define a translator that maps a clojure defrecord to a java class."
  [name fNames cljName]
  (let [gsNames (map getSet-postfix fNames)
	fields (map #(format "(.get%s jRec) " %) gsNames)
	fields (apply str fields)]
    (format javaToCljFmt name cljName fields
	    name name)))

(defn gen-clj
  "Generate a .clj file with the clojure definitions."
  [package name fields cljName]
  (let [head  (clj-head package name)
	fNames (map first fields)
	defrecStr (clj-defrecord cljName fNames)
	defToJavaStr (clj-to-java name fNames cljName)
	createStr  (clj-create name fNames cljName)
	cloneStr  (clj-clone name fNames)
	defToCljStr  (java-to-clj name fNames cljName)
	fileName (concat (list "src")
			 (str/split package #"\.")
			 (list "clj" (str name ".clj")))
	fileName (apply str (interpose File/separator fileName))
	]
    (write-src-file fileName (str head
				  defrecStr createStr cloneStr
				  defToJavaStr defToCljStr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to generate mapping that is used by hiberate.
;;

;; these mapping are use to over-rule the default-mapping
;; of hibernate
;;    - String --> text  such that longvarchar is used
;;    - Date --> timestamp   for maximal precision.
(def hibTypeMapping {"String"  "text"
		     "java.util.Date" "timestamp"
		     "Date"   "timestamp"})
		     

(def hibHeadFmt "<hibernate-mapping package=\"%s\">
\t<class name=\"%s\" table=\"%s\">
\t\t<id name=\"id\" column=\"id\">
\t\t\t<generator class=\"native\"/>
\t\t</id>\n")
(def hibFootFmt "\t</class>\n</hibernate-mapping>\n\n")

(defn gen-hib [package tbl_name fields name]
  (letfn [(hib-descr[[nm type]]
		    (let [tpStr (if-let [htype (hibTypeMapping type)]
				  (str " type=\"" htype "\"") "")]
		      (format "\t\t<property name=\"%s\"%s/>\n" nm tpStr)))]
    (let [head (format hibHeadFmt package
		       name (str/lower-case tbl_name))
	  ;; skip the 'id' fields (is already pre-defined)
	  fields (skip-id fields ffirst)
	  fldStr (map hib-descr fields)
	  fldStr (apply str fldStr)
	  fileName `("resources"  ~(str tbl_name ".xml.include"))
	  fileName (apply str (interpose File/separator fileName))]
      (write-src-file fileName (str head fldStr hibFootFmt)))))




(defn generate-hibernate-code
  "Generate the code for a hibernate connector. This consists of:
    1. A java file defining the java-object that will be stored.
    2. A clojure file with the corresponding defrecord and helper functions.
    3. A hibernate mapping (inclusion) file."
  [qualifiedName fields]
  ;; translate all keywords to strings first.
  (println "received fields: " fields)
  (let [qualifiedName (name qualifiedName)
	fields (map #(map name  %) fields)]
;;	fields (map #(vector (name (first %)) (name (second %))) fields)]
    (println)
    (println "switched fields: " fields)
    (let [nameParts (str/split qualifiedName #"\.")
	  name  (last nameParts)
	  package (take (dec (count nameParts)) nameParts)
	  package (apply str (interpose "." package))
	  cljName (str name cljSuffix)
	  tblName  (str @tablePrefix name)
	  ;; prepend an id-fields
	  fields  (concat (list ["id" "Long" "protected"]) fields)]
      (gen-java package name fields)
      (gen-clj package name fields cljName)
      (gen-hib package tblName fields name)
    )))

