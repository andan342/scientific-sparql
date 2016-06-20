;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2010-16 Andrej Andrejev, UDBL
;;; $RCSfile: turtle-reader.lsp,v $
;;; $Revision: 1.27 $ $Date: 2014/01/17 18:25:56 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: Streaming RDF Data Cube adapter
;;; =============================================================
;;; $Log: turtle-reader.lsp,v $
;;;
;;; =============================================================

;Reserved URIs
(defparameter a-uri (make-uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))

;Class URIs
(defparameter dataset-curi (make-uri "http://purl.org/linked-data/cube#DataSet"))
(defparameter dsd-curi (make-uri "http://purl.org/linked-data/cube#DataStructureDefinition"))
(defparameter slice-curi (make-uri "http://purl.org/linked-data/cube#Slice"))
(defparameter obs-curi (make-uri "http://purl.org/linked-data/cube#Observation"))

;Property URIs
(defparameter structure-puri (make-uri "http://purl.org/linked-data/cube#structure"))
(defparameter slice-puri (make-uri "http://purl.org/linked-data/cube#slice"))
(defparameter component-puri (make-uri "http://purl.org/linked-data/cube#component"))
(defparameter dimension-puri (make-uri "http://purl.org/linked-data/cube#dimension"))
(defparameter measure-puri (make-uri "http://purl.org/linked-data/cube#measure"))
(defparameter order-puri (make-uri "http://purl.org/linked-data/cube#order"))
(defparameter attachment-puri (make-uri "http://purl.org/linked-data/cube#componentAttachment"))
(defparameter obs-puri (make-uri "http://purl.org/linked-data/cube#observation"))
(defparameter dataset-puri (make-uri "http://purl.org/linked-data/cube#dataSet"))

;ASSUMPTIONS (for speed and simplicity): 
; - (?x a ?class) triples precede other triples with subject ?x
; - qb:DataSet, qb:DataStructureDefiniton and component definitions precede qb:Slice and qb:Observation triples
; - (?obs ?component ?value) triples follow immediately after (?obs a qb:Observation) triple for the respective ?obs
; - similarly for (?slice ?component ?value) and (?slice qb:observation ?obs) for the respective ?slice
; - slice-attached qb:Observation triples follow the respective qb:Slice definition

;TODO: a more expensive reader without these assumptions

(defstruct dc-dataset dsd 
  sfields ofields ; component-based fields - assigned in FINALIZE-DSD
  slices observations ; lists of slies and observations with actual data
  slice-uris) ; lists of slice and observation uris

(defstruct dc-dsd dims measures ;dimensions (sorted) and measures - components assigned in FINALIZE-DSD
  component-uris) ;temporary list of component nodes - emptied in FINALIZE-DSD

(defstruct dc-component name kind order slice-attached dimpos)

(defstruct dc-field name pos dimpos (type :integer) (values (make-hash-table :test #'equal)) valcnt nma slice-val)

(defstruct dc-slice values observations obs-uris)

(defun emit-tr (trs tr)
  (osql-result trs (first tr) (second tr) (third tr)))

; Step I routines

(defun finalize-dsd (dsd components)
  "Distribute COMPONENT objects into DIMS and MEASURES"
  (dolist (c-uri (dc-dsd-component-uris dsd)) ; join DSD-COMPONENT-URIS and COMPONENTS by URI
    (let ((c (cdr (assoc c-uri components)))) 
      (when c
	(selectq (dc-component-kind c) ; assign to DIMS or MEASURES
		 (:dim (push c (dc-dsd-dims dsd)))
		 (:measure (push c (dc-dsd-measures dsd)))
		 t))))
  (setf (dc-dsd-dims dsd) ; sort DSD dimensions according to the specified order, dimensions without order come last
	(sort (dc-dsd-dims dsd)
	      (f/l (a b) (and (dc-component-order a) 
			      (or (null (dc-component-order b)) 
				  (< (dc-component-order a) (dc-component-order b)))))))
  (setf (dc-dsd-component-uris dsd) nil)) ; indicate DSD is finalized

(defun add-field (ds f c)
  (if (dc-component-slice-attached c)
      (progn
	(setf (dc-field-pos f) (length (dc-dataset-sfields ds)))
	(push f (dc-dataset-sfields ds)))
    (progn
      (setf (dc-field-pos f) (length (dc-dataset-ofields ds)))
      (push f (dc-dataset-ofields ds)))))

(defun prepare-dataset (ds components)
  "Create field objects according to componens, finalize DSD first if not finalized"
  (unless (or (dc-dataset-ofields ds) (dc-dataset-sfields ds))
    (when (dc-dsd-component-uris (dc-dataset-dsd ds))
      (finalize-dsd (dc-dataset-dsd ds) components))
    (let (f (dimpos -1))
      (dolist (c (dc-dsd-dims (dc-dataset-dsd ds)))
	(setq f (make-dc-field :name (dc-component-name c) :dimpos (incf dimpos)))
	(setf (dc-component-dimpos c) dimpos)
	(add-field ds f c))
      (dolist (c (dc-dsd-measures (dc-dataset-dsd ds)))
	(setq f (make-dc-field :name (dc-component-name c) :pos))
	(add-field ds f c)))))
    
(defun field-by-name (name fields)
  (dolist (f fields nil)
    (when (equal name (dc-field-name f)) (return f))))

(defun update-field (f value)
  (unless (gethash value (dc-field-values f))
    (puthash value (dc-field-values f) t)) ; will be changed to integers on step II
   (unless (integerp value)
     (if (numberp value)  ;TODO: support more RDF types
	 (when (eq (dc-field-type f) :integer) 
	   (setf (dc-field-type f) :real))
       (setf (dc-field-type f) :other))))
	    
; Step II routines

(defun uri< (a b)
  (string< (uri-id a) (uri-id b)))

(defun enumerate-values (f trs)
  (let (vals (cnt -1))
    (maphash (f/l (k v) (push k vals)) (dc-field-values f)) ; get all distinct values into VALS list
    (setq vals (sort vals (if (eq (dc-field-type f) :other) #'uri< #'<))) ;TODO: implement general RDF sorter 
    (when (dc-field-dimpos f) ; if a dimensions field
      (unless vals (error (concat "Dimension has no values: " (dc-field-name f))))
      (setf (dc-field-valcnt f) (length vals)) ; compute size of extent
      (when (and (eq (dc-field-type f) :integer) (= (first vals) 0) (= (car (last vals)) (1- (dc-field-valcnt f))))
	(setf (dc-field-values f) nil))) ; dimension is integer and dense - remove the hash table
    (when (dc-field-values f) 
      (print (list 'fname= (dc-field-name f) 'vals= vals))
      (dolist (v vals) ; enumerate distinct values
	(setf (gethash v (dc-field-values f)) (incf cnt)))
      ; TODO: emit VALS as an RDF collection
      )))  

;; Step IV

(defun value-to-idx (f value)
  "Map the value using the field's HT, if present"
  (if (dc-field-values f)
      (gethash value (dc-field-values f))
    value))

(defun idxs-set (idxs f obs)
  "Set IDX to the mapped value from OBS, corresponding to the field's POS"
  (setf (aref idxs (dc-field-dimpos f)) (value-to-idx f (aref obs (dc-field-pos f)))))

(defun scan-observations (observations idxs ds)
  "Scan the list of observations, IDX may contain mapped values from slice, fill NMA on measures"
  (dolist (obs observations)
    (dolist (f (dc-dataset-ofields ds)) ; Update IDXS for dimension fields
      (when (dc-field-dimpos f)
	(idxs-set idxs f obs)))
    (dolist (f (dc-dataset-ofields ds)) ; Updata NMA for measure fields
      (unless (dc-field-dimpos f)
	(nma-set (dc-field-nma f) (arraytolist idxs) (value-to-idx f (aref obs (dc-field-pos f))))))
    (dolist (f (dc-dataset-sfields ds)) ; Update NMA for slice-attached measure fields
      (unless (dc-field-dimpos f)
	(nma-set (dc-field-nma f) (arraytolist idxs) (dc-field-slice-val f))))))

(defun dca-+++ (fno trs rs rp ro)
  "RDF Data Cube adapter - to be used as dca(triples(<filename>))"
  (let (datasets ds dsds dsd components (slice-cnt 0) (obs-cnt 0) slice slice-uri obs obs-uri f)
    ;; I. Process the incomping triples
    (mapbag trs (f/l (tr) 
		     (cond ((equal (second tr) a-uri) ; Class definitions
;			    (print (list 'a-tr= tr)) ;DEBUG
			    (cond ((equal (third tr) dataset-curi) ; '?x a qb:DataSet
				   (unless (assoc (first tr) datasets)
				     (push (cons (first tr) (make-dc-dataset)) datasets)
				     (emit-tr trs tr)))
				  ((equal (third tr) dsd-curi)  ; ?x a qb:DataStructureDefinition
				   (unless (assoc (first tr) dsds)
				     (let ((new-dsd (make-dc-dsd)))
				       (push (cons (first tr) new-dsd) dsds)
				       (dolist (ds-r datasets)
;					 (print (list 'ds-r= ds-r)) ;DEBUG
					 (when (equal (dc-dataset-dsd (cdr ds-r)) (first tr))
					   (setf (dc-dataset-dsd (cdr ds-r)) new-dsd))))
				     (emit-tr trs tr)))
				  ((equal (third tr) slice-curi) ; ?x a qb:Slice - consume
				   (incf slice-cnt)
				   (setq ds (dolist (ds-r datasets nil) ; lookup the dataset
					      (when (member (first tr) (dc-dataset-slice-uris (cdr ds-r)))
						(return (cdr ds-r)))))
				   (unless ds (error (concat "No qb:DataSet instance contains the slice: " (uri-id (first tr)))))
				   (prepare-dataset ds components)
				   (setq slice-uri (first tr))
				   (setq slice (make-dc-slice :values (make-array (length (dc-dataset-sfields ds)))))
				   (push slice (dc-dataset-slices ds))) ; both SLICE and DS are set here
				  ((equal (third tr) obs-curi) ; ?x a qb:Observation - consume
				   ; assuming SLICE is set or NIL, DS is set if SLICE is set
				   (incf obs-cnt)
				   (if (and slice (member (first tr) (dc-slice-obs-uris slice)))
				       (progn
					 (setq obs (make-array (length (dc-dataset-ofields ds))))
					 (push obs (dc-slice-observations slice)))
				     (setq obs t)) ; temporary value - obs will be created on qb:dataSet property
				   (setq obs-uri (first tr)))
				  (t (emit-tr trs tr))))

			   ((equal (first tr) obs-uri) ; Observation attributes - consume
			    ; if OBS <> T assuming DS is set
			    (cond ((equal (second tr) dataset-puri) ; ?obs qb:dataSet ?x
				   (when (eq obs t)
				     (setq ds (cdr (assoc (third tr) datasets)))
				     (unless ds (error (concat "qb:Observation refers to unknown dataset: " (third tr))))
				     (prepare-dataset ds components)
				     (setq obs (make-array (length (dc-dataset-ofields ds))))
				     (push obs (dc-dataset-observations ds))))
				  ((setq f (field-by-name (second tr) (dc-dataset-ofields ds))) ; component values
				   (update-field f (third tr))
				   (setf (aref obs (dc-field-pos f)) (third tr)))))

			   ((equal (first tr) slice-uri) ; Slice attributes - consume
			    ; assuming DS, SLICE, SLICE-URI are set
			    (cond ((equal (second tr) obs-puri) ; ?slice qb:observation ?x
				   (push (third tr) (dc-slice-obs-uris slice)))
				  ((setq f (field-by-name (second tr) (dc-dataset-sfields ds))) ; component values
				   (update-field f (third tr))
				   (setf (aref (dc-slice-values slice) (dc-field-pos f)) (third tr)))))

			   ((setq ds (cdr (assoc (first tr) datasets))) ; Dataset attributes
			    (cond ((equal (second tr) structure-puri) ; ?ds qb:structure ?x				  
				   (setq dsd (cdr (assoc (third tr) dsds)))
				   (setf (dc-dataset-dsd ds) (if dsd dsd (third tr)))
				   (emit-tr trs tr))
				  ((equal (second tr) slice-puri) ; ?ds qb:slice ?x - consume
				   (push (third tr) (dc-dataset-slice-uris ds)))
				  (t (emit-tr trs tr))))

			   ((and (equal (second tr) component-puri) (setq dsd (cdr (assoc (first tr) dsds)))) ; ?dsd qb:component ?x
			    (push (third tr) (dc-dsd-component-uris dsd))
			    (emit-tr trs tr))

			   ((member (second tr) (list dimension-puri measure-puri order-puri attachment-puri)) ; Component attributes
;			    (print (list 'component-triple (first tr) (third tr))) ;DEBUG
			    (let ((c (cdr (assoc (first tr) components))))
			      (unless c
				(setq c (make-dc-component))
				(push (cons (first tr) c) components))
			      (cond ((equal (second tr) order-puri) ; ?c qb:order ?x
				     (setf (dc-component-order c) (third tr)))
				    ((equal (second tr) attachment-puri) ; ?c qb:componentAttachment ?x 
				     (when (equal (third tr) slice-curi)
				       (setf (dc-component-slice-attached c) t)))
				    (t ; ?c qb:dimension|qb:measure ?x
				     (setf (dc-component-name c) (third tr))
				     (setf (dc-component-kind c) (if (equal (second tr) dimension-puri) :dim :measure)))))
			    (emit-tr trs tr))

			   (t (emit-tr trs tr))))) ; pass on all other triples

   (dolist (ds-r datasets)
     ; II. Enumerate values for each field, or remove hash tables if no enumeration needed
     (let ((fields (append (dc-dataset-sfields (cdr ds-r)) (dc-dataset-ofields (cdr ds-r))))
	   dims idxs value)
       (dolist (f fields)
	 (if (dc-field-dimpos f) (enumerate-values f trs)
	   (if (member (dc-field-type f) '(:integer :real)) ; measure fields
	       (setf (dc-field-values f) nil) ; remove HT - measure is numeric
	     (enumerate-values f trs))))
       
       ; III. Allocate the arrays
       (setq dims (mapcar (f/l (c) (dc-field-valcnt (field-by-name (dc-component-name c) fields))) 
			  (dc-dsd-dims (dc-dataset-dsd (cdr ds-r))))) ; contruct array shape (same for each measure)
       (dolist (f fields)
	 (unless (dc-field-dimpos f) ; for each measure field
	   (setf (dc-field-nma f) (make-nma (if (eq (dc-field-type f) :real) 1 0) dims)))) 
       
       ; IV. Fill the arrays
       (setq idxs (make-array (length dims)))
       (if (dc-dataset-slices (cdr ds-r))
	   (dolist (slice (dc-dataset-slices (cdr ds-r)))
	     (dolist (f (dc-dataset-sfields (cdr ds-r)))
	       (if (dc-field-dimpos f) 
		   (idxs-set idxs f (dc-slice-values slice)) ; store slice-attached dimension in IDXS
		 (setf (dc-field-slice-val f) (value-to-idx f (aref (dc-slice-values slice) (dc-field-pos f)))))) ; store slice-attached measure
	     (scan-observations (dc-slice-observations slice) idxs (cdr ds-r)))
	 (scan-observations (dc-dataset-observations (cdr ds-r)) idxs (cdr ds-r)))
     
       ; V. Emit the arrays
       (dolist (f fields)
	 (unless (dc-field-dimpos f)
	   (emit-tr trs (list (car ds-r) (dc-field-name f) (dc-field-nma f)))))
;       (pps (cdar datasets)) ;DEBUG
	 
;    (print (list 'datasets= datasets 'dsds= dsds 'components= components 'slices slice-cnt 'obss obs-cnt)) ;DEBUG
       ))))

(osql "
create function DCA(Bag of (Literal, Literal, Literal) trs) -> Bag of (Literal, Literal, Literal)
  as foreign 'dca-+++';
")

; (osql "DCA(turtle('complete.ttl'));")