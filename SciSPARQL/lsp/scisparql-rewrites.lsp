;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2015 Andrej Andrejev, UDBL
;;; $RCSfile: scisparql-rewrites.lsp,v $
;;; $Revision: 1.1 $ $Date: 2015/10/21 17:02:02 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: Amos rewrites useful for SciSPARQL query processing
;;; =============================================================
;;; $Log: scisparql-rewrites.lsp,v $
;;; Revision 1.1  2015/10/21 17:02:02  andan342
;;; Added normalization 'rewrite' to avoid combinatorial explosion on chains of rdf:first/rdf:rest
;;;
;;; =============================================================


;; 1. Avoiding combinatorial explosion when normalizing chains of rdf:first and rdf:rest

(defun filter-or-branches (p) 
  (cond ((atom p) p)
	((eq (car p) 'or) (cons 'or (mapfilter #'validate-disjunct (cdr p))))
	(t p)))

(defun validate-disjunct (p)
  (cond ((atom p) t)
	((eq (car p) 'and) (null (intersection (literal-or-nil-vars (cdr p)) (subject-or-predicate-vars (cdr p)))))
	(t t)))

(defparameter _literal-or-nil-valued-fns_ '(LITERAL.RDF:FIRST->LITERAL LITERAL.RDF:REST->LITERAL))

(defparameter _triple-store-fns_ '(P_INTEGER.GRAPH->LITERAL.LITERAL.LITERAL))

(defun literal-or-nil-vars (ps)
  (let (res)
    (dolist (p ps)
      (when (and (eq (typename (car p)) 'oid)
		 (member (getobject (car p) 'name) _literal-or-nil-valued-fns_)
		 (symbolp (third p))
		 (not (member (third p) res)))
	(push (third p) res)))
    res))

(defun subject-or-predicate-vars (ps)
  (let (res)
    (dolist (p ps)
      (when (and (eq (typename (car p)) 'oid)
		 (member (getobject (car p) 'name) _triple-store-fns_))
	(when (and (symbolp (third p)) (not (member (third p) res))) 
	  (push (third p) res))
	(when (and (symbolp (fourth p)) (not (member (fourth p) res))) 
	  (push (fourth p) res))))
    res))		 	

(advise-around #'normalizepred '(filter-or-branches *))
