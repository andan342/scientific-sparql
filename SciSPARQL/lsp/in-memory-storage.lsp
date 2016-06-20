;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2013 Andrej Andrejev, UDBL
;;; $RCSfile: in-memory-storage.lsp,v $
;;; $Revision: 1.6 $ $Date: 2015/11/18 13:25:04 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: In-memory storage facilities for SSDM
;;; =============================================================
;;; $Log: in-memory-storage.lsp,v $
;;; Revision 1.6  2015/11/18 13:25:04  andan342
;;; Added option to define cost and cardinality model for foreign functions,
;;; Added multidirectional foreign functions in SciSPARQL
;;;
;;; Revision 1.5  2015/03/25 15:02:43  andan342
;;; No more underfined functions in string-based version
;;;
;;; Revision 1.4  2015/03/19 20:37:44  andan342
;;; in-memory-get-blank lispfn now conforms the naming convention
;;;
;;; Revision 1.3  2014/01/23 20:57:13  andan342
;;; Update back-end interface to accommodate for named graphs and SPARQL Updates
;;;
;;; Revision 1.2  2014/01/21 22:10:07  andan342
;;; - Added streamed updates: INSERT DATA / DELETE DATA (during parsing phase)
;;; - support for prefixed URI:s (and even variables!) in FROM clause
;;; - Added parametrized updates (DEFINE PROCEDURE)
;;; - eval-time NGDict lookup in function/procedure bodies
;;;
;;; Revision 1.1  2013/12/20 14:48:21  andan342
;;; Supporting multiple stored graphs:
;;; - added GRAPH and FROM NAMED syntax
;;; - LOAD() and CLEAR() functions now take a graph URI as the last argument
;;; - default stored graph is now in GRAPH(0)
;;;
;;;
;;; =============================================================

;; Depends on:
;;  _sq_string_based_ _sq_basetype_

; (defvar _sq_default_triples_fn_ "GRAPH(0)")

(if _sq_string_based_
(osql "
create function GRAPH(Integer g) -> Bag of (Charstring s, Charstring p, Charstring o);

create function NGDict(Charstring uri) -> Integer i;

create function GRAPHS(Vector of integer vg) -> Bag of (Charstring s, Charstring p, Charstring o)
  as select s, p, o from Integer g
      where (s, p, o) in GRAPH(g)
        and g in vg;

/*
create function GRAPH_insert(Integer g, Charstring s, Charstring p, Charstring o) -> Boolean
  as add GRAPH(g) = (s,p,o);
*/

create function TermInGRAPHS(Charstring x) -> Boolean
  /* Lookup function for blanks */
  as some(select x
            from Integer g, Charstring s, Charstring p, Charstring o
           where (s,p,o) in GRAPH(g)
             and ((s = x) or (p = x) or (o = x)));
")
(osql "
create function GRAPH(Integer g) -> Bag of (Literal s, Literal p, Literal o);

create function NGDict(Literal uri) -> Integer i;

create function GRAPHS(Vector of integer vg) -> Bag of (Literal s, Literal p, Literal o)
  as select s, p, o from Integer g
      where (s, p, o) in GRAPH(g)
        and g in vg;

/*
create function GRAPH_insert(Integer g, Literal s, Literal p, Literal o) -> Boolean
  as add GRAPH(g) = (s,p,o);
*/

create function TermInGRAPHS(URI x) -> Boolean
  /* Lookup function for blanks */
  as some(select x
            from Integer g, Literal s, Literal p, Literal o
           where (s,p,o) in GRAPH(g)
             and ((s = x) or (p = x) or (o = x)));
"))

(osql "
create_index('graph','g','hash','multiple');
create_index('graph','s','hash','multiple');
create_index('graph','p','hash','multiple');
create_index('graph','o','hash','multiple');
")

(defun in-memory-get-blank (x)
  "Check if string X represnets a node inside the tripels"
  (getfunction (car (getobject (getfunctionnamed 'TermInGRAPHS) 'resolvents)) 
	       (list (if _sq_string_based_ x (URI x)))))

(setq _sq_get_blank_ #'in-memory-get-blank)

(defparameter graph-dict-fn (car (getobject (getfunctionnamed 'NGDict) 'resolvents)))

(defun graph-p (graph)
  (if _sq_string_based_ (not (string= graph "")) (eq (typename graph) 'uri)))

(defun ngdict-lookup (graph add-missing)
  "Lookup a graph id in NGDict, add if missing and allowed to add, signal error otherwise"  
  (let ((res (car (getfunction-firsttuple graph-dict-fn (list graph) t))))
    (unless res
      (if add-missing
	  (progn
	    (setq res 1)
	    (dolist (row (extent graph-dict-fn))
	      (when (>= (second row) res)
		(setq res (1+ (second row)))))
	    (addfunction graph-dict-fn (list graph) (list res)))
	(error (concat "Graph " (if _sq_string_based_ graph (concat "<" (uri-id graph) ">")) " not found!")))) ;TODO: maybe should silently return an empty graph!
    res))

(defun NGDictAdd-+ (fno graph res)
  (when (graph-p graph)
    (osql-result graph (ngdict-lookup graph t))))

(if _sq_string_based_
    (osql "create function NGDictAdd(Charstring graph) -> Integer as foreign 'NGDictAdd-+';")
  (osql "create function NGDictAdd(Literal graph) -> Integer as foreign 'NGDictAdd-+';"))

;;;;;;IN-MEMORY variants of TRIPLES-FN translations

(defparameter graph-fn (theresolvent 'GRAPH))

(defun in-memory-graph-to-id-tr (graph-tr add-missing delayed) 
  "In-memory variant for graph to graph-id translation"
  (if delayed 
      (concat (if add-missing "NGDictAdd(" "NGDict(") graph-tr ")")
    (ngdict-lookup (if _sq_string_based_ graph-tr (uri graph-tr)) add-missing)))

(defun in-memory-gs-to-triples-fn (gs)
  "In-memory variant for graph-ids to triples-fn translation"
  (cond ((null gs) "GRAPH(0)")
	((cdr gs) (concat "GRAPHS({" (strings-to-string (mapcar #'mkstring gs) "" "," "") "})"))
	(t (concat "GRAPH(" (car gs) ")"))))

; Get Amos syntax for translating graph uri to graph id, NIL is translated to default graph 
; if DELAYED, graph-tr can be either variable name (e.g. "source") or URI constructor call "URI('http://source')",
; if not DELAYED, graph-tr should be a string uri, e.g. "http://source"
(defvar _sq_graph_to_id_tr_ #'in-memory-graph-to-id-tr) 

; Get Amos syntax for translating graph uri to graph id, NIL is translated to default graph 
; if DELAYED, graph-tr can be either variable name (e.g. "source") or URI constructor call "URI('http://source')",
;    as achieved by applying RESOURCE-STRING-TO-AMOSQL
; if not DELAYED, graph-tr should be a string uri, e.g. "http://source"
(defvar _sq_gs_to_triples_fn_ #'in-memory-gs-to-triples-fn) 

;;;;;;IN-MEMORY CLEAR, INSERT AND REMOVE 

(defun in-memory-clear-all ()
  "In-memory variant of Amos syntax for clearAll()"
  (parse (concat "remove GRAPH(c:g) = (c:s,c:p,c:o) from Integer c:g, " 
		 (strings-to-string '(" c:s" " c:p" " c:o") _sq_basetype_ ", " "") ";") t))

(defvar _sq_clear_all_ #'in-memory-clear-all)

(defun in-memory-clear (g)
  "In-memory variant of Amos syntax for clear()"
  (delfunction graph-fn (list g) t))

(defvar _sq_clear_ #'in-memory-clear)

(defun in-memory-insert (g s p o)
  (addfunction graph-fn (list g) (list s p o) t))

(defvar _sq_insert_ #'in-memory-insert)

(defun in-memory-remove (g s p o)
  (remfunction graph-fn (list g) (list s p o) t))

(defvar _sq_remove_ #'in-memory-remove)

