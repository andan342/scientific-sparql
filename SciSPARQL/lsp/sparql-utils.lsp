;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2009-2011 Andrej Andrejev, UDBL
;;; $RCSfile: sparql-utils.lsp,v $
;;; $Revision: 1.30 $ $Date: 2015/06/04 09:15:45 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: Utility functions used in AmosQL translations of SparQL queries
;;; =============================================================
;;; $Log: sparql-utils.lsp,v $
;;; Revision 1.30  2015/06/04 09:15:45  andan342
;;; Fixed string-based loader
;;;
;;; Revision 1.29  2015/04/21 08:34:46  andan342
;;; Fixed bsection-related bug
;;;
;;; Revision 1.28  2015/04/19 14:36:27  andan342
;;; Enabled general numeric expressions as LIMIT and OFFSET parameters
;;;
;;; Revision 1.27  2015/03/25 15:02:43  andan342
;;; No more underfined functions in string-based version
;;;
;;; Revision 1.26  2014/02/24 16:50:46  andan342
;;; Fixed bugs in rdf:dump()
;;;
;;; Revision 1.25  2014/01/23 20:57:14  andan342
;;; Update back-end interface to accommodate for named graphs and SPARQL Updates
;;;
;;; Revision 1.24  2014/01/21 22:10:09  andan342
;;; - Added streamed updates: INSERT DATA / DELETE DATA (during parsing phase)
;;; - support for prefixed URI:s (and even variables!) in FROM clause
;;; - Added parametrized updates (DEFINE PROCEDURE)
;;; - eval-time NGDict lookup in function/procedure bodies
;;;
;;; Revision 1.23  2014/01/17 00:33:34  andan342
;;; Added full support for effective boolean values (EBV), fixed minor bugs
;;;
;;; Revision 1.22  2014/01/09 13:43:11  andan342
;;; Added DELETE/INSERT updates, as specified in http://www.w3.org/TR/2013/REC-sparql11-update-20130321/
;;;
;;; Revision 1.21  2013/12/20 14:48:23  andan342
;;; Supporting multiple stored graphs:
;;; - added GRAPH and FROM NAMED syntax
;;; - LOAD() and CLEAR() functions now take a graph URI as the last argument
;;; - default stored graph is now in GRAPH(0)
;;;
;;; Revision 1.20  2013/12/16 14:04:57  andan342
;;; Added subqueries, SELECT *, EXISTS, NOT EXISTS, ans ASK syntax
;;;
;;; Revision 1.19  2013/09/06 21:47:24  andan342
;;; - Moved definitions of standard SPARQL functions to sparql-fns.lsp, SciSPARQL functions to scisparql-fns.lsp
;;; - Implemented all SPARQL 1.1 string functions (except REPLACE, ENCODE_FOR_URI, langMAtches)
;;; - Redefined rdf:regex() using Amos functions like() and like_i()
;;;
;;; Revision 1.18  2013/01/15 21:55:29  andan342
;;; Added more efficient and safe versions of ARGMIN and ARGMAX
;;;
;;; Revision 1.17  2013/01/10 12:19:34  andan342
;;; REGEX now handles both W3C standard and legacy (TopicMap) regular expression syntax
;;;
;;; Revision 1.16  2013/01/09 11:46:26  andan342
;;; Full support for string-based version
;;;
;;; Revision 1.15  2012/11/19 23:58:09  andan342
;;; - using resolve-nma inside all array-processing functions as part of proxy-allowing polymorphic behavior,
;;; - moved _nma_proxy_threshold_ to core SSDM, setting this value in regression test return NMAs from queries,
;;; - added _nma_limit_ for a max NMA size to be retrieved, printing a warning if exceeded
;;;
;;; Revision 1.14  2012/11/05 23:10:42  andan342
;;; LOAD() functition now reads remote Turtle files via HTTP
;;;
;;; Revision 1.13  2012/06/25 20:36:35  andan342
;;; Added TypedRDF and support for custom types in Turtle reader and SciSPARQL queries
;;; - rdf:toTypedRDF and rdf:strdt can be used as constructors in terms of RDF literals
;;; - rdf:str and rdf:datatype can be used as field accessors
;;;
;;; Revision 1.12  2012/06/24 15:00:16  andan342
;;; Fixed typecheck bug in rdf:regex
;;;
;;; Revision 1.11  2012/06/06 13:09:44  andan342
;;; String-based mode added for compliance with SWARD/SARD tests. 'string-based-wrapper.lsp' file should be loaded on topof Amos2.exe - no separate executable required, no SPARQL console enabled.
;;;
;;; Revision 1.10  2012/04/27 10:16:09  torer
;;; optional() -> optional0()
;;;
;;; Revision 1.9  2012/03/17 14:23:38  andan342
;;; Added DUMP() function to dump current triple store into Turtle file
;;;
;;; Revision 1.8  2012/02/23 19:15:39  andan342
;;; - Using _sq_ prefix for all SSDM switches, changed how _sq_default_triples_fn_ is used,
;;; - _sq_load_triples_ doesn't have to check for file existance,
;;; - URI-id function made reversible
;;;
;;; Revision 1.7  2012/02/10 11:47:50  andan342
;;; All Amos functions implementing SciSPARQL functions now have/get rdf: namespace
;;;
;;; Revision 1.1  2006/02/12 20:01:09  torer
;;; Folder AmosNT/headers contains CVS header templates
;;;
;;; =============================================================

;; Depends on:
;; _sq_string_based_

(load "sparql-fns.lsp")

(unless _sq_string_based_
  (load "scisparql-fns.lsp"))

;; ------------------- INSERT, REMOVE, SOURCE, GENERIC LOAD & CLEAR -----------------

;; these are based on implementation anchors defined in in-memory-storage.lsp
;; along with in-memory implementations there

(defun graph-to-g (graph add-missing)
  "Transform graph-denoting literal into translated Amos expression yielding graph id"
  (if (graph-p graph) 
    (funcall _sq_graph_to_id_tr_ (if _sq_string_based_ graph (uri-id graph)) add-missing nil) 0))

(defun resource-string-to-amosql (str)
  "Transform resource into its Amos representation"
  (if _sq_string_based_ (concat "'" str "'") (concat "URI('" str "')")))

(defun rdf-insert---- (fno g s p o) ;TODO: should be all-literals!
  (funcall _sq_insert_ g s p o))

(defun rdf-remove---- (fno g s p o) ;TODO: should be all-literals!
  (funcall _sq_remove_ g s p o))

(defun rdf-source- (fno filename) 
  (parse-file (if _sq_string_based_ filename (ustr-str filename)) "SPARQL"))

(defvar *sq-readers* '(("ttl" . turtle) ("nt" . turtle)))
		
(defun rdf-load--- (fno filename-or-url replace graph)
  (let* ((g (graph-to-g graph t))
	 (filename (rdf-to-string filename-or-url))
	 (ext (substring (1+ (string-rightpos filename '("."))) (1- (length filename)) filename))
	 (reader-fn (cdr (assoc ext *sq-readers*))))
    (unless reader-fn (error (concat "unsupported file format: " ext)))
    (unless (rdf-not replace)
      (funcall _sq_clear_ g))
    (mapfunction (car (getobject (getfunctionnamed reader-fn) 'resolvents)) (list filename-or-url)
		 (f/l (row) (apply _sq_insert_ (cons g row))))))

(defun rdf-clear- (fno graph)
  (funcall _sq_clear_ (graph-to-g graph t)))

(defun rdf-clearAll (fno)
  (funcall _sq_clear_all_ nil))

(if _sq_string_based_
(osql "
create function rdf:insert(Integer g, Charstring s, Charstring p, Charstring o) -> Boolean as foreign 'rdf-insert----';
create function rdf:remove(Integer g, Charstring s, Charstring p, Charstring o) -> Boolean as foreign 'rdf-remove----';

create function rdf:source(Charstring filename) -> Boolean 
  as foreign 'rdf-source-';

create function rdf:load(Charstring filename_or_url, Charstring replace, Charstring graph) -> Boolean
  as foreign 'rdf-load---';

create function rdf:load(Charstring filename_or_url, Charstring replace) -> Boolean
  as rdf:load(filename_or_url, replace, '');

create function rdf:load(Charstring filename_or_url) -> Boolean 
  as rdf:load(filename_or_url, '', '');

create function rdf:clear(Charstring graph) -> Boolean 
  as foreign 'rdf-clear-';
")
(osql "
create function rdf:insert(Integer g, Literal s, Literal p, Literal o) -> Boolean as foreign 'rdf-insert----';
create function rdf:remove(Integer g, Literal s, Literal p, Literal o) -> Boolean as foreign 'rdf-remove----';

create function rdf:source(Literal filename) -> Boolean 
  as foreign 'rdf-source-';

create function rdf:load(Literal filename_or_url, Literal replace, Literal graph) -> Boolean
  as foreign 'rdf-load---';

create function rdf:load(Literal filename_or_url, Literal replace) -> Boolean
  as rdf:load(filename_or_url, replace, 0);

create function rdf:load(Literal filename_or_url) -> Boolean 
  as rdf:load(filename_or_url, 0, 0);

create function rdf:clear(Literal graph) -> Boolean 
  as foreign 'rdf-clear-';
"))

(osql "
create function rdf:clear() -> Boolean
  as rdf:clear('');

create function rdf:clearAll() -> Boolean
  as foreign 'rdf-clearAll';
")

;; ---------------- DUMP (only available in RDF-based version) ----------

(unless _sq_string_based_

(defun rdf-dump (term outs) 
  "Print RDF resource to Turtle file" ;TODO: support N-Triples as well
  (selectq (typename term)
	   (ustr (formatl outs "\"" (ustr-str term) "\"") ; base string
		 (when (> (length (ustr-lang term)) 0) ; unless langtag is empty
		   (formatl outs "@" (ustr-lang term)))) ; concatenate langtag
	   (uri (if (and (>= (length (uri-id term)) 2) 
			 (string= (substring 0 1 (uri-id term)) "_:"))
		    (formatl outs (uri-id term)) ; notation for blank nodes
		  (formatl outs "<" (uri-id term) ">"))) ; notation for URIs
	   (typedrdf (formatl outs "\"" (typedrdf-str term) "\"^^") ; notation for typed literals
		     (rdf-dump (typedrdf-typeuri term) outs)) ;recursive
	   (nma (nma-dump (apr term) outs)) ; print array as list
	   (formatl outs (rdf-to-string term)))) ; use rdf:str() function in all other cases
	   
(defun dump-triples-- (fno filename triples)
  (let ((outs (openstream filename "w")) (cnt 0))
    (unwind-protect
	(progn
	  (mapbag triples (f/l (triple)
			       (dolist (term triple)
				 (rdf-dump term outs)
				 (formatl outs " "))
			       (formatl outs "." t)
			       (incf cnt)))      
	  (print (concat cnt " triples written to file: " filename)))
      (closestream outs))))

(defun rdf-dump- (fno filename)
  (parse (concat "dump_triples('" 
		 (if (member (typename filename) '(string ustr))
		     (rdf-to-string filename)
		   (error "Can only dump to a local file specified by a string"))
		 "'," (funcall _sq_gs_to_triples_fn_ '("0")) ");") t))

(osql "
create function dump_triples(Charstring filename, Bag of (Literal, Literal, Literal) triples) -> Boolean
  as foreign 'dump-triples--';

create function rdf:dump(Literal filename) -> Boolean
  as foreign 'rdf-dump-';
")

) ;of !string-based

;;--------------------- Other utilities ----------------------

(unless _sq_string_based_

(defun roundto-all (x to)
  "Apply roundto to all real numbers in list, recursively"
  (cond ((floatp x) 
	 (roundto x to))
	((and (eq (typename x) 'nma) (> (nma-kind x) 0))
	 (nma-roundto x to))
	((consp x) 
	 (cons (roundto-all (car x) to) 
	       (roundto-all (cdr x) to)))
	(t x)))

) ; of !string-based

(defun getgraph (resource)
  "Get the contents of a named graph identified by RESOURCE string (NIL for default graph"
   (eval `(osql ,(concat (funcall _sq_gs_to_triples_fn_ 
				  (when resource (list (funcall _sq_graph_to_id_tr_ resource nil nil)))) ";"))))


;;--------------------------- Bag sections

(set-resulttypesfn 
 (if _sq_string_based_
     (osql "create function bsection1(Bag b, Charstring start, Charstring stop) -> Bag of Object
             as bsection(b, atoi(start), atoi(stop));")
   (osql "create function bsection1(Bag b, Literal start, Literal stop) -> Bag of Object
           as bsection(b, cast(start as Number), cast(stop as Number));"))
 'transparent-collection-resulttypes)
	   