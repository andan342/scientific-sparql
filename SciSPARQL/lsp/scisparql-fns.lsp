;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2010-2013 Andrej Andrejev, UDBL
;;; $RCSfile: scisparql-fns.lsp,v $
;;; $Revision: 1.5 $ $Date: 2015/05/23 12:07:45 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: Specific SciSPARQL functions
;;; =============================================================
;;; $Log: scisparql-fns.lsp,v $
;;; Revision 1.5  2015/05/23 12:07:45  andan342
;;; Introduced A() array comprehension, ARRAY(), MAP(), and CONDENSE() 2nd-order functions into SciSPARQL
;;;
;;; Revision 1.4  2015/05/22 21:48:44  andan342
;;; Added nma_create(), nma_map(), nma_condense() 2nd-order functions, a() array comprehension
;;;
;;; Revision 1.3  2015/05/22 10:15:55  andan342
;;; added Partial Tuple type - used as closure implemenation for array mappers
;;;
;;; Revision 1.2  2014/10/07 14:42:15  andan342
;;; No error on empty query, SPARQL function ListPrefixes() added
;;;
;;; Revision 1.1  2013/09/06 21:47:23  andan342
;;; - Moved definitions of standard SPARQL functions to sparql-fns.lsp, SciSPARQL functions to scisparql-fns.lsp
;;; - Implemented all SPARQL 1.1 string functions (except REPLACE, ENCODE_FOR_URI, langMAtches)
;;; - Redefined rdf:regex() using Amos functions like() and like_i()
;;;
;;; Revision 1.1  2006/02/12 20:01:09  torer
;;; Folder AmosNT/headers contains CVS header templates
;;;
;;; =============================================================

;; Depends on:
;; _sq_string_based_

;; --------------- 2ND-ORDER FUNCTIONS --------------------------------

(defun rdf-argmin-+ (fno bag)
  (let (resrow)
    (mapbag bag (f/l (row) (when (or (null resrow) (< (second row) (second resrow)))
			     (setq resrow row))))
    (when resrow
      (osql-result bag (first resrow)))))

(defun rdf-argmax-+ (fno bag)
  (let (resrow)
    (mapbag bag (f/l (row) (when (or (null resrow) (> (second row) (second resrow)))
			     (setq resrow row))))
    (when resrow
      (osql-result bag (first resrow)))))

(osql "
create function rdf:argmin(Bag b) -> Literal
  as foreign 'rdf-argmin-+';

create function rdf:argmax(Bag b) -> Literal
  as foreign 'rdf-argmax-+';
")

(defun rdf-listprefixes++ (fno prefix uri)
  (dolist (prefix-rec *session-prefixes*)
    (osql-result (ustr (car prefix-rec)) (ustr (cdr prefix-rec)))))

(osql "
create function rdf:ListPrefixes() -> Bag of (Literal, Literal)
  as foreign 'rdf-listprefixes++';
")

;;------------- ARRAY COMPREHENSION --------------------------------

(defun a-+ (fno x res)   
  (let ((xlist (arraytolist x))
	kind res)
    (dolist (e xlist)
      (selectq (typename e)
	       (integer (unless kind (setq kind 0)))
	       (real (setq kind 1))
	       (error "Invalid array element" e)))
    (setq res (make-nma kind (list (length x))))
    (nma-fill res xlist)
    (osql-result x res)))

(osql "
create function a(Vector of Literal) -> NMA
  as foreign 'a-+';
")

;;------------- CLOSURES AND MAPPERS ----------------------------------

;(CREATETYPE 'PartialTuple ()))
;(createliteraltype 'PartialTuple nil 'PartialTuple 'empty-print)
(createliteraltype 'PartialTuple '(literal) 'PartialTuple 'empty-print)


(defun uri2etype-+ (fno typeuri etype) ;TODO: support COMPLEX
  (unless (eq (typename typeuri) 'uri)
    (error "Element type should be URI" typeuri))
  (osql-result typeuri (if (xmls-integertype-p (uri-id typeuri)) 0
			 (if (xmls-realtype-p (uri-id typeuri)) 1
			   (error "Unsupported element type" typeuri)))))

(osql "
create function make_partial_tuple(Vector values, Vector of Integer fills) -> PartialTuple
  as foreign 'make_partial_tuple--+';

create function uri2etype(Literal typeuri) -> Integer
  as foreign 'uri2etype-+';

create function nma_create(Integer kind, Literal shape, Function fn, PartialTuple pt) -> NMA
  as foreign 'nma_create----+';

create function rdf:array(Literal typeuri, Literal shape, Function fn, PartialTuple pt) -> Literal
  /* No special case in SciSPARQL translation: closure is always translated to a pair of function and make_partial_tuple() call */
  as nma_create(uri2etype(typeuri), shape, fn, pt);

create function nma_map(Integer kind, Vector of Literal xs, Function fn, PartialTuple pt) -> NMA
  as foreign 'nma_map----+';

create function nma_condense(Integer op, Literal x, Function fn, PartialTuple pt) -> Literal
  as foreign 'nma_condense----+';
")

