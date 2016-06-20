;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2009-2014 Andrej Andrejev, UDBL
;;; $RCSfile: sparql-grammar.lsp,v $
;;; $Revision: 1.45 $ $Date: 2016/01/28 23:25:36 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: SciSPARQL grammar in format accepted by GRM parser generator
;;; =============================================================
;;; $Log: sparql-grammar.lsp,v $
;;; Revision 1.45  2016/01/28 23:25:36  andan342
;;; Fixed recently introuced bug with parameterized updates
;;;
;;; Revision 1.44  2015/11/18 13:25:04  andan342
;;; Added option to define cost and cardinality model for foreign functions,
;;; Added multidirectional foreign functions in SciSPARQL
;;;
;;; Revision 1.43  2015/11/07 18:20:56  andan342
;;; Added syntax for induced array operations like .+, .<, .&, etc.
;;; Added syntax for specifying cost and fanout for foreign UDFs
;;;
;;; Revision 1.42  2015/04/21 08:34:45  andan342
;;; Fixed bsection-related bug
;;;
;;; Revision 1.41  2015/02/18 12:35:17  andan342
;;; - SciSPARQL functions and procedures of 0 arguments can now be defined
;;; - fixed bug when identifying refered variables in single-BIND blocks
;;;
;;; Revision 1.40  2015/01/28 17:35:22  andan342
;;; - added SAMPLE(), GROUP_CONCAT(), COUNT(*), COUNT(DISTINCT *)
;;; - SUM(), AVG(), MIN(), MAX() now do not return if incompatible values are found in bag
;;; - MATLAB range syntax is now default
;;;
;;; Revision 1.39  2014/09/17 19:53:13  andan342
;;; - added floor() and ceil() SPARQL functions
;;; - added VALUES feature
;;;
;;; Revision 1.38  2014/03/31 15:20:53  andan342
;;; Added Path Expressions syntax into SciSPARQL grammar (no translation yet)
;;;
;;; Revision 1.37  2014/02/23 21:18:42  andan342
;;; - added ORDER BY, OFFSET, LIMIT translations,
;;; - raising errors on unknown prefixes, misplaced aggregate functions
;;;
;;; Revision 1.36  2014/02/18 15:38:48  andan342
;;; Parsing queries with ORDER BY / OFFSET / LIMIT
;;;
;;; Revision 1.35  2014/01/21 22:10:08  andan342
;;; - Added streamed updates: INSERT DATA / DELETE DATA (during parsing phase)
;;; - support for prefixed URI:s (and even variables!) in FROM clause
;;; - Added parametrized updates (DEFINE PROCEDURE)
;;; - eval-time NGDict lookup in function/procedure bodies
;;;
;;; Revision 1.34  2014/01/10 11:25:44  andan342
;;; Added ARCHIVE queries from A-SPARQL using archive_content_schema() function
;;;
;;; Revision 1.33  2014/01/09 13:43:09  andan342
;;; Added DELETE/INSERT updates, as specified in http://www.w3.org/TR/2013/REC-sparql11-update-20130321/
;;;
;;; Revision 1.32  2013/12/20 14:48:22  andan342
;;; Supporting multiple stored graphs:
;;; - added GRAPH and FROM NAMED syntax
;;; - LOAD() and CLEAR() functions now take a graph URI as the last argument
;;; - default stored graph is now in GRAPH(0)
;;;
;;; Revision 1.31  2013/12/16 14:04:55  andan342
;;; Added subqueries, SELECT *, EXISTS, NOT EXISTS, ans ASK syntax
;;;
;;; Revision 1.30  2013/12/03 20:49:16  andan342
;;; Added unary + operator
;;;
;;; Revision 1.29  2013/09/27 22:05:41  andan342
;;; Fixed bug with TL funcalls introduced by previous grammar update
;;;
;;; Revision 1.28  2013/09/22 13:23:33  andan342
;;; Added IN and NOT IN operators
;;;
;;; Revision 1.27  2013/01/22 16:21:32  andan342
;;; Added (sparql-add-extender-engine ...) to add engines (and new ways to define foreign functions) at runtime
;;;
;;; Revision 1.26  2012/12/14 14:02:03  andan342
;;; Added rdf:insert and rdf:clear Amos functions, callable from SciSPARQL
;;;
;;; Revision 1.25  2012/06/15 16:14:38  andan342
;;; Added ARGMIN and ARGMAX second-order functions, brief and complete f(*) syntax for closures,
;;; all new variables now contain ':' to avoid conflicts with user variables,
;;; a mechanism introduced to generate new variables and conditions from inside expression translator.
;;;
;;; Revision 1.24  2012/06/06 13:09:44  andan342
;;; String-based mode added for compliance with SWARD/SARD tests. 'string-based-wrapper.lsp' file should be loaded on topof Amos2.exe - no separate executable required, no SPARQL console enabled.
;;;
;;; Revision 1.23  2012/05/26 11:45:37  andan342
;;; Introduced CONSTRUCT into the new version of the parser
;;;
;;; Revision 1.22  2012/02/08 16:18:15  andan342
;;; Now using READ-TOKEN-based SPARQL lexer in Turtle/NTriples reader,
;;; moved (returtle-amosfn ...) def to master.lsp
;;;
;;; Revision 1.21  2012/02/02 22:52:35  andan342
;;; Enabled session-wide PREFIX statements,
;;; fixed bugs with:
;;; - 'total' aggregates in non-vectorized mode,
;;; - floating-point number reader,
;;; - default step for array projections in Python-syntax mode
;;;
;;; Revision 1.20  2012/02/01 10:59:09  andan342
;;; Added BIND syntax from W3C SPARQL 1.1,
;;; made '.' optional between conditions in the block
;;;
;;; Revision 1.1  2006/02/12 20:01:09  torer
;;; Folder AmosNT/headers contains CVS header templates
;;;
;;; =============================================================

(load (concat (getenv "AMOS_HOME") "/lsp/grm/ascend.lsp"))

;;(load "sparql-lsp-parser-datamodel.lsp")

(defparameter sparql-grammar '((<tl-stat> -> <simple-fncall> #'(lambda (a) (list nil (cons 'call a))))
			       (<tl-stat> -> quit #'(lambda (a b) '(nil (quit))))
			       (<tl-stat> -> lisp #'(lambda (a b) '(nil (lisp))))
			       (<tl-stat> -> <prefix> #'(lambda (a) (list nil (cons 'prefix a))))
			       (<tl-stat> -> <prefixed-stat> 1)
			       (<prefixed-stat> -> <query> #'(lambda (a) (list nil a))) 
			       (<prefixed-stat> -> <update> #'(lambda (a) (list nil a))) 
			       (<prefixed-stat> -> <streamed-update> #'(lambda (a) (list nil a)))
			       (<prefixed-stat> -> <archive> #'(lambda (a) (list nil a))) 
			       (<prefixed-stat> -> <define> #'(lambda (a) (list nil a))) 
			       (<prefixed-stat> -> <prefix> <prefixed-stat> #'(lambda (a b) (push a (first b)) b)) 			       			       
			       (<prefix> -> prefix pref uri-tail uri #'(lambda (a b c d) (car (push (cons b d) (sparql-data-prefixes data))))) ; PREFIX p: <..>, uri-tail is empty
			       (<prefix> -> prefix uri-tail uri #'(lambda (a b c) (car (push (cons "" c) (sparql-data-prefixes data))))) ; PREFIX : <..>
			       (<query> -> select <sel-list> #'(lambda (a b) (make-sparql-stat :type 'select :what b))) 
			       (<query> -> select distinct <sel-list> #'(lambda (a b c) (make-sparql-stat :type 'select :what c :distinct t))) 
			       (<query> -> select times #'(lambda (a b) (make-sparql-stat :type 'select :what '(asterisk))))
			       (<query> -> select distinct times #'(lambda (a b c) (make-sparql-stat :type 'select :what '(asterisk) :distinct t))) 
			       (<query> -> construct <block> #'(lambda (a b) (make-sparql-stat :type 'construct :what b)))
			       (<query> -> ask <block> #'(lambda (a b) (make-sparql-stat :type 'ask :what b)))
			       (<query> -> <query> from <resource> #'(lambda (a b c) (push c (sparql-stat-from a)) a)) 
			       (<query> -> <query> from named <resource> #'(lambda (a b c d) (push d (sparql-stat-from a)) a)) 
			       (<query> -> <query> where <block> #'(lambda (a b c) (setf (sparql-stat-where a) c) a)) 
			       (<query> -> <query> <block> #'(lambda (a b) (setf (sparql-stat-where a) b) a)) ;; WHERE keyword can be omitted
			       (<query> -> <query> <values-clause> #'(lambda (a b) (unless (sparql-stat-where a) (error "VALUES shold follow a query block!"))
								       (setf (block-conds (sparql-stat-where a)) 
									     (append (block-conds (sparql-stat-where a)) (list b))) a))
			       (<query> -> <query> group by <var-list> #'(lambda (a b c d) (setf (sparql-stat-groupby a) d) a)) 
			       (<query> -> <query> having <expr> #'(lambda (a b c) (setf (sparql-stat-having a) c) a)) 
			       (<query> -> <query> order by <order-list> #'(lambda (a b c d) (setf (sparql-stat-orderby a) d) a))
			       (<query> -> <query> offset <num-expr> #'(lambda (a b c) (setf (sparql-stat-offset a) c) a))
			       (<query> -> <query> limit <num-expr> #'(lambda (a b c) (setf (sparql-stat-limit a) c) a))
			       (<update0> -> delete #'(lambda (a) (make-sparql-update :delete 'asterisk)))
			       (<update0> -> delete <block> #'(lambda (a b) (make-sparql-update :delete b)))
			       (<update0> -> insert <block> #'(lambda (a b) (make-sparql-update :insert b)))
			       (<update0> -> delete <block> insert <block> #'(lambda (a b c d) (make-sparql-update :delete b :insert d)))
			       (<update> -> with <resource> <update0> #'(lambda (a b c) (push b (sparql-update-with c)) c))
			       (<update> -> <update0> 1)
			       (<update> -> <update> using <resource> #'(lambda (a b c) (push c (sparql-update-using a)) a))
			       (<update> -> <update> using named <resource> #'(lambda (a b c d) (push d (sparql-update-using a)) a))
			       (<update> -> <update> where <block> #'(lambda (a b c) (setf (sparql-update-where a) c) a))
			       (<streamed-op> -> insert #'(lambda (a) (setf (sparql-data-streamed data) 'streamed-insert)))
			       (<streamed-op> -> delete #'(lambda (a) (setf (sparql-data-streamed data) 'streamed-delete)))
			       (<streamed-update> -> <streamed-op> data <block> #'(lambda (a b c) a))
			       (<archive0> -> archive as string comma string #'(lambda (a b c d e) (make-sparql-archive :as (cons c e))))
			       (<archive0> -> <archive0> from uri #'(lambda (a b c) (push c (sparql-archive-from a)) a))
			       (<archive0> -> <archive0> triples <block> where <block> #'(lambda (a b c d e) (push (cons c e) (sparql-archive-triples a)) a))
			       (<archive0> -> <archive0> triples <block> #'(lambda (a b c) (push (cons c nil) (sparql-archive-triples a)) a))
			       (<archive> -> <archive0> 1)
			       (<archive> -> <archive> union triples <block> where <block> #'(lambda (a b c d e f) (push (cons d f) (sparql-archive-triples a)) a))
			       (<archive> -> <archive> union triples <block> #'(lambda (a b c d) (push (cons d nil) (sparql-archive-triples a)) a))

			       (<order-list> -> <ordered-var> #'(lambda (a) (list a)))
			       (<order-list> -> <ordered-var> <order-list> #'(lambda (a b) (cons a b)))
			       (<ordered-var> -> var #'(lambda (a) (cons 'var a)))
			       (<ordered-var> -> desc left-par var right-par #'(lambda (a b c d) (cons 'desc-var c)))

			       (<sel-list> -> <named-expr-or-var> #'(lambda (a) (list a))) 
			       (<sel-list> -> <named-expr-or-var> <sel-list> #'(lambda (a b) (cons a b))) 
			       (<named-expr-or-var> -> var #'(lambda (a) (cons 'var a))) 
			       (<named-expr-or-var> -> <named-expr> 1)
			       (<named-expr> -> left-par <expr> as var right-par #'(lambda (a b c d e) (list 'named d b)))
			       (<var-list> -> #'(lambda (a) nil))
			       (<var-list> -> var <var-list> #'(lambda (a b) (cons a b)))
			       (<block> -> left-brace <conds> right-brace #'(lambda (a b c) (make-block :conds b)))

			       (<conds> -> #'(lambda () nil))
			       (<conds> -> <triples> <conds> #'(lambda (a b) (if (sparql-data-streamed data) (streamed-exec data a) (cons a b))))
			       (<conds> -> <nontriples> <conds> #'(lambda (a b) (if (and (sparql-data-streamed data) (not (eq (car a) 'graph-block)))
										    (error "Only triple patterns are allowed in streamed updates!")
										  (cons a b))))
			       (<conds> -> dot <conds> 2)			       
			       
			       (<nontriples> -> filter left-par <expr> right-par #'(lambda (a b c d) (cons 'filter c)))
			       (<nontriples> -> filter <simple-fncall> #'(lambda (a b) (cons 'filter b)))
			       (<nontriples> -> filter exists <block> #'(lambda (a b c) (cons 'exists c)))
			       (<nontriples> -> filter not exists <block> #'(lambda (a b c d) (cons 'not-exists d)))
			       (<nontriples> -> optional <block> #'(lambda (a b) (cons 'optional b)))
			       (<nontriples> -> <union> 1)
			       (<nontriples> -> bind <named-expr> #'(lambda (a b) (cons 'bind (cdr b))))
			       (<nontriples> -> <values-clause> 1)			      
			       (<nontriples> -> left-brace <query> right-brace #'(lambda (a b c) (cons 'subquery b)))
;			       (<nontriples> -> graph <resource> <block> #'(lambda (a b c) (list 'graph-block b c)))
;			       (<nontriples> -> graph var <block> #'(lambda (a b c) (list 'graph-block (cons 'var b) c)))
			       (<nontriples> -> <graph-specifier> <block> #'(lambda (a b) (pop (sparql-data-graphs data)) (list 'graph-block a b)))
			       (<graph-specifier> -> graph <resource> #'(lambda (a b) (car (push b (sparql-data-graphs data)))))
			       (<graph-specifier> -> graph var #'(lambda (a b) (if (sparql-data-streamed data) 
										   (error "Variables are not allowed in streamed updates!") 
										 (cons 'var b))))

			       (<union> -> <block> union <block> #'(lambda (a b c) (list 'union a c)))
			       (<union> -> <union> union <block> #'(lambda (a b c) (append a (list c)))) 

			       (<values-clause> -> values var left-brace <inline-data> right-brace 
						#'(lambda (a b c d e) (list 'values (list b) (mapcar #'list d))))
			       (<values-clause> -> values left-par <var-list> right-par left-brace <inline-data-tuples> right-brace
						#'(lambda (a b c d e f g) (list 'values c f)))
			       (<inline-data> -> #'(lambda () nil))
			       (<inline-data> -> <resource> <inline-data> #'(lambda (a b) (cons a b)))
			       (<inline-data> -> <literal> <inline-data> #'(lambda (a b) (cons a b)))
			       (<inline-data> -> UNDEF <inline-data> #'(lambda (a b) (cons '(undef) b)))
			       (<inline-data-tuples> -> #'(lambda () nil))
			       (<inline-data-tuples> -> left-par <inline-data> right-par <inline-data-tuples> #'(lambda (a b c d) (cons b d)))

			       (<triples> -> <node> <pred-obj-list> #'(lambda (a b) (cons 'triples (sparql-make-triples a b)))) 
			       (<pred-obj-list> -> <object-or-path> <obj-list> #'(lambda (a b) (make-sqo :n (mapcar (f/l (o) (list a (sqo-n o))) (nreverse b)) 
												 :ts (mapcan #'sqo-ts b)))) ; merge triples lists
			       (<pred-obj-list> -> <pred-obj-list> semicolon <object-or-path> <obj-list> 
						#'(lambda (a b c d) (make-sqo :n (append (sqo-n a) (mapcar (f/l (o) (list c (sqo-n o))) (nreverse d)))
									      :ts (append (sqo-ts a) (mapcan #'sqo-ts d)))))
			       (<object-or-path> -> var #'(lambda (a) (cons 'var a)))
			       (<object-or-path> -> <path-alternative> 1)
			       (<path-alternative> -> <path-sequence> 1)
			       (<path-alternative> -> <path-alternative> pipe <path-sequence> #'(lambda (a b c) (if (eq (car a) 'path-alt)
														    (append a (list c))
														  (list 'path-alt a c))))
			       (<path-sequence> -> <path-elt-or-inverse> 1)
			       (<path-sequence> -> <path-sequence> divide <path-elt-or-inverse> #'(lambda (a b c) (if (eq (car a) 'path-seq)
														      (append a (list c))
														    (list 'path-seq a c))))
			       (<path-elt-or-inverse> -> <path-elt> 1)
			       (<path-elt-or-inverse> -> cap <path-elt> #'(lambda (a b) (list 'path-inverse b)))
			       (<path-elt> -> <path-primary> 1)
			       (<path-elt> -> <path-primary> question #'(lambda (a b) (list 'path01 a)))
			       (<path-elt> -> <path-primary> times #'(lambda (a b) (list 'path0n a)))
			       (<path-elt> -> <path-primary> plus #'(lambda (a b) (list 'path1n a)))
			       (<path-primary> -> <resource> 1)
			       (<path-primary> -> left-par <path-alternative> right-par 2)
			       
			       (<obj-list> -> <node> #'(lambda (a) (list a))) 
			       (<obj-list> -> <obj-list> comma <node> #'(lambda (a b c) (cons c a))) 
			       
			       (<node> -> <object> #'(lambda (a) (make-sqo :n a))) 
			       (<node> -> left-bracket <pred-obj-list> right-bracket 
						    #'(lambda (a b c) (let ((res (sparql-gen-blank data)))
									(make-sqo :n res :ts (sparql-make-triples (make-sqo :n res) b)))))
			       (<node> -> left-bracket right-bracket #'(lambda (a b) (make-sqo :n (sparql-gen-blank data)))) 

			       (<node> -> left-par <nodes> right-par #'(lambda (a b c) (sparql-collection-to-triples data (nreverse b)))) 
			       (<nodes> -> #'(lambda () nil)) 
			       (<nodes> -> <nodes> <node> #'(lambda (a b) (cons b a))) 
			       
			       (<object> -> <rdf-term> 1) 
			       (<object> -> var #'(lambda (a) (cons 'var a))) 
			       (<rdf-term> -> underscore uri-tail #'(lambda (a b) (list 'blank b))) 
			       (<rdf-term> -> <resource> 1) 
			       (<object> -> <literal> 1) 

			       (<resource> -> uri #'(lambda (a) (list 'uri a))) 
			       (<resource> -> pref uri-tail #'(lambda (a b) (list 'prefixed a b))) 
			       (<resource> -> uri-tail #'(lambda (a) (list 'prefixed "" a))) 
			       (<resource> -> a #'(lambda (a) (list 'uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))) 
			       (<literal> -> number #'(lambda (a) (list 'number a))) 
			       (<literal> -> true #'(lambda (a) '(true))) 
			       (<literal> -> false #'(lambda (a) '(false))) 
			       (<literal> -> <string-lang> 1) 
			       (<literal> -> <string-lang> double-cap <resource>  
					  #'(lambda (a b c) (list 'typed (second a) (third a) c)))
			       (<string-lang> -> string #'(lambda (a) (list 'ustr a nil))) 
			       (<string-lang> -> string at-id #'(lambda (a b) (list 'ustr a b))) 

			       (<expr> -> <expr> or <conjunction> #'(lambda (a b c) (list 'or a c))) 
			       (<expr> -> <conjunction> 1) 
			       (<expr> -> id #'(lambda (a) (cons (cons 'id a) '((asterisk)))))
			       (<expr> -> times #'(lambda (a) '(asterisk)))
			       (<conjunction> -> <conjunction> and <rel-expr> #'(lambda (a b c) (list 'and a c))) 
			       (<conjunction> -> <rel-expr> 1) 
			       (<rel-expr> -> <num-expr-or-uri> 1) 
			       (<rel-expr> -> <num-expr-or-uri> <compare-op> <num-expr-or-uri> #'(lambda (a b c) (list b a c))) 
			       (<rel-expr> -> <num-expr-or-uri> in <expr-list-par>  #'(lambda (a b c) (list 'in a c)))
			       (<rel-expr> -> <num-expr-or-uri> not in <expr-list-par>  #'(lambda (a b c d) (list 'not (list 'in a d))))
			       (<compare-op> -> not-equal #'(lambda (a) '!=)) 
			       (<compare-op> -> equal #'(lambda (a) '=)) 
			       (<compare-op> -> less #'(lambda (a) '<)) 
			       (<compare-op> -> greater #'(lambda (a) '>)) 
			       (<compare-op> -> less-or-equal #'(lambda (a) '<=)) 
			       (<compare-op> -> greater-or-equal #'(lambda (a) '>=)) 
			       (<compare-op> -> anot-equal #'(lambda (a) 'a!=)) 
			       (<compare-op> -> aequal #'(lambda (a) 'a=)) 
			       (<compare-op> -> aless #'(lambda (a) 'a<)) 
			       (<compare-op> -> agreater #'(lambda (a) 'a>)) 
			       (<compare-op> -> aless-or-equal #'(lambda (a) 'a<=)) 
			       (<compare-op> -> agreater-or-equal #'(lambda (a) 'a>=)) 

			       (<num-expr-or-uri> -> <num-expr> 1) 
			       (<num-expr-or-uri> -> <rdf-term> 1) 

			       (<num-expr> -> <mult-expr> plus <mult-expr> #'(lambda (a b c) (list '+ a c))) 
			       (<num-expr> -> <mult-expr> minus <mult-expr> #'(lambda (a b c) (list '- a c))) 
			       (<num-expr> -> <mult-expr> aplus <mult-expr> #'(lambda (a b c) (list 'a+ a c))) 
			       (<num-expr> -> <mult-expr> aminus <mult-expr> #'(lambda (a b c) (list 'a- a c))) 
			       (<num-expr> -> <mult-expr> a-or <mult-expr> #'(lambda (a b c) (list 'a-or a c))) 
			       (<num-expr> -> <mult-expr> 1) 
			       (<mult-expr> -> <unary-expr> times <unary-expr> #'(lambda (a b c) (list '* a c))) 
			       (<mult-expr> -> <unary-expr> divide <unary-expr> #'(lambda (a b c) (list '/ a c))) 
			       (<mult-expr> -> <unary-expr> atimes <unary-expr> #'(lambda (a b c) (list 'a* a c))) 
			       (<mult-expr> -> <unary-expr> adivide <unary-expr> #'(lambda (a b c) (list 'a/ a c))) 
			       (<mult-expr> -> <unary-expr> a-and <unary-expr> #'(lambda (a b c) (list 'a-and a c))) 
			       (<mult-expr> -> <unary-expr> 1) 
			       (<unary-expr> -> <prim-expr> 1) 
			       (<unary-expr> -> minus <prim-expr> #'(lambda (a b) (list 'u- b))) 
			       (<unary-expr> -> plus <prim-expr> 2)
			       (<prim-expr> -> left-par <expr> right-par #'(lambda (a b c) b)) 
			       (<prim-expr> -> <literal> 1) 
			       (<prim-expr> -> not <prim-expr> #'(lambda (a b) (list 'not b))) 
			       (<prim-expr> -> <var-or-fncall> 1) 
       			       (<var-or-fncall> -> var #'(lambda (a) (cons 'var a))) 
			       (<var-or-fncall> -> <fncall> 1) 
			       (<simple-fncall> -> id <expr-list-par> #'(lambda (a b) (cons (cons 'id a) b)))
			       (<fncall> -> <simple-fncall> 1)
			       (<fncall> -> uri-cast <expr> right-par #'(lambda (a b c) (list (list 'uri a) b))) 
			       (<fncall> -> pref uri-tail-cast <expr> right-par 
					 #'(lambda (a b c d) (list (list 'prefixed a b) c)))
			       (<fncall> -> uri-tail-cast <expr> right-par 
					 #'(lambda (a b c) (list (list 'prefixed "" a) b)))
			       (<fncall> -> <var-or-fncall> left-bracket <range-expr-list> right-bracket #'(lambda (a b c d) (cons 'aref (cons a c)))) 

			       (<expr-list> -> <expr> #'(lambda (a) (list a))) 
			       (<expr-list> -> <expr> comma <expr-list> #'(lambda (a b c) (cons a c))) 
			       (<expr-list-par> -> left-par <expr-list> right-par #'(lambda (a b c) b))
			       (<expr-list-par> -> left-par distinct <expr> right-par #'(lambda (a b c d) (list 'distinct c)))
			       (<expr-list-par> -> left-par right-par #'(lambda (a b) nil))

      			       (<range-expr> -> colon #'(lambda (a) (list 'range0))) 
			       (<range-expr> -> <num-expr> 1) 
			       (<range-expr> -> <num-expr> colon <num-expr> #'(lambda (a b c) (list 'range2 a c))) 
			       (<range-expr> -> colon <num-expr> #'(lambda (a b) (list 'range2 '(number "0") b))) 
			       (<range-expr> -> <num-expr> colon #'(lambda (a b) (list 'range2 a '(number "-1")))) 
			       (<range-expr> -> <num-expr> colon <num-expr> colon <num-expr> #'(lambda (a b c d e) (list 'range3 a c e))) 
			       (<range-expr> -> colon <num-expr> colon <num-expr> #'(lambda (a b c d) (list 'range3 '(number "0") b d))) 
			       (<range-expr> -> <num-expr> colon <num-expr> colon #'(lambda (a b c d) (list 'range3 a c '(number "-1")))) 
			       (<range-expr> -> colon <num-expr> colon #'(lambda (a b c) (list 'range3 '(number "0") b '(number "-1")))) 
			       (<range-expr> -> colon colon <num-expr> #'(lambda (a b c) (list 'range3 '(number "0") '(number "-1") c))) 
			       (<range-expr> -> <num-expr> colon colon <num-expr> #'(lambda (a b c d) (list 'range3 a '(number "-1") d))) 

			       (<range-expr-list> -> <range-expr> #'(lambda (a) (list a))) 
			       (<range-expr-list> -> <range-expr> comma <range-expr-list> #'(lambda (a b c) (cons a c))) 
			       
			       (<define> -> define function id left-par <var-list> right-par as <decl-body-or-multi> 
					 #'(lambda (a b c d e f g h) (make-define-stat :name c :agg nil :vars e :defs h)))
			       (<define> -> define aggregate id left-par var right-par as <decl-body> 
					 #'(lambda (a b c d e f g h) (make-define-stat :name c :agg t :vars (list e) :defs (list (cons nil h)))))
			       (<define> -> define procedure id left-par <var-list> right-par as <update>
					 #'(lambda (a b c d e f g h) (make-define-stat :name c :agg nil :vars e :defs (list (list nil 'sparql h)))))
			       (<foreign-decl-body> -> id string #'(lambda (a b) (list a b)))
			       (<foreign-decl-body> -> lisp string #'(lambda (a b) (list "lisp" b)))
			       (<decl-body> -> <foreign-decl-body> 1)			       
			       (<decl-body> -> <foreign-decl-body> cost number fanout number #'(lambda (a b c d e) (append a (list (cons c e)))))
			       (<decl-body> -> <query> #'(lambda (a) (list 'sparql a)))
			       (<decl-body-or-multi> -> <decl-body> #'(lambda (a) (list (cons nil a))))
			       (<decl-body-or-multi> -> <decl-multibody> 1)
			       (<decl-multibody> -> for string <decl-body> #'(lambda (a b c) (list (cons b c))))
			       (<decl-multibody> -> <decl-multibody> for string <decl-body> #'(lambda (a b c d) (cons (cons c d) a)))
			       ))

(make-slr1-parser (grammar-from-johnsons sparql-grammar) '(semicolon) "sparql-slr1" "sparql-slr1.lsp")
