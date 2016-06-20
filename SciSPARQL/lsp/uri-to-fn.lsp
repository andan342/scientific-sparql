;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2009-2014 Andrej Andrejev, UDBL
;;; $RCSfile: uri-to-fn.lsp,v $
;;; $Revision: 1.9 $ $Date: 2014/01/21 22:10:09 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: RDF datasource URI translation to Amos functions
;;; =============================================================

;; Depends on:
;; _sq_string_based_

(defun uri-to-amos-function (graphs)
  "Translate URI to Amos function storing the tripes"
  (let ((oid (and (getfunctionnamed 'upv t) 
		  (get-most-specific-resolvent 'upv '(Charstring))))
	restypenames res)
    (when oid 
      (setq restypenames (mapcar (f/l (type) (getobject type 'name)) (getobject oid 'restypes)))
      (cond ((equal restypenames '(charstring object)) ;; SWARD-specific mapping
	     (setq res (dolist (mapping (extent oid))
			 (when (string= (third mapping) graphs) (return (concat (first mapping) "()"))))))

	    ((equal restypenames '(charstring)) ;; TopicMap-specific mapping
	     (setq res (dolist (mapping (extent oid))
			 (when (string= (first mapping) graphs) (return (concat "#" (second mapping) "()")))))
	     (unless res (setq res "tm_rdf_triples_let()"))) ;;default mapping
	    )
      res)))


