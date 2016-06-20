;;; ============================================================
;;; AMOS2
;;; 
;;; Author: (c) 2009-2014 Andrej Andrejev, UDBL
;;; $RCSfile: sparql-stream-parser.lsp,v $
;;; $Revision: 1.31 $ $Date: 2016/06/20 09:05:27 $
;;; $State: Exp $ $Locker:  $
;;;
;;; Description: SparQL stream parser
;;; =============================================================
;;; $Log: sparql-stream-parser.lsp,v $
;;; Revision 1.31  2016/06/20 09:05:27  andan342
;;; - SSDM is now maintaining its own version of parse-utils.lsp
;;; - presence of python_ext.dll is checked before loading Python extension
;;;
;;; Revision 1.30  2015/11/18 13:25:05  andan342
;;; Added option to define cost and cardinality model for foreign functions,
;;; Added multidirectional foreign functions in SciSPARQL
;;;
;;; Revision 1.29  2015/11/07 18:20:57  andan342
;;; Added syntax for induced array operations like .+, .<, .&, etc.
;;; Added syntax for specifying cost and fanout for foreign UDFs
;;;
;;; Revision 1.28  2014/03/31 15:20:54  andan342
;;; Added Path Expressions syntax into SciSPARQL grammar (no translation yet)
;;;
;;; Revision 1.27  2014/03/28 17:27:19  andan342
;;; Fixed problem with '.' directly following prefixed URIs in triples
;;;
;;; Revision 1.26  2014/02/18 15:38:49  andan342
;;; Parsing queries with ORDER BY / OFFSET / LIMIT
;;;
;;; Revision 1.25  2014/01/21 22:10:08  andan342
;;; - Added streamed updates: INSERT DATA / DELETE DATA (during parsing phase)
;;; - support for prefixed URI:s (and even variables!) in FROM clause
;;; - Added parametrized updates (DEFINE PROCEDURE)
;;; - eval-time NGDict lookup in function/procedure bodies
;;;
;;; Revision 1.24  2014/01/17 18:25:55  andan342
;;; Fixed more bugs, using common XMLS type vocabulary in rdf-types.lsp
;;;
;;; Revision 1.23  2014/01/17 00:33:33  andan342
;;; Added full support for effective boolean values (EBV), fixed minor bugs
;;;
;;; Revision 1.22  2014/01/10 11:25:45  andan342
;;; Added ARCHIVE queries from A-SPARQL using archive_content_schema() function
;;;
;;; Revision 1.21  2014/01/09 13:43:10  andan342
;;; Added DELETE/INSERT updates, as specified in http://www.w3.org/TR/2013/REC-sparql11-update-20130321/
;;;
;;; Revision 1.20  2013/12/16 00:13:07  andan342
;;; Generating more informative syntax error messages
;;;
;;; Revision 1.19  2013/09/29 15:32:32  andan342
;;; Added langMatches() as specified in http://www.ietf.org/rfc/rfc4647.txt
;;; Added sameTerm() as synonym for equality
;;;
;;; Revision 1.18  2013/09/22 13:23:33  andan342
;;; Added IN and NOT IN operators
;;;
;;; Revision 1.17  2013/01/28 09:22:13  andan342
;;; Added MOD and DIV functions, cost hints, fixed bug when reading URIs with comma, TODO comments to improve error reporting
;;;
;;; Revision 1.16  2013/01/22 16:21:32  andan342
;;; Added (sparql-add-extender-engine ...) to add engines (and new ways to define foreign functions) at runtime
;;;
;;; Revision 1.15  2012/11/05 23:10:42  andan342
;;; LOAD() functition now reads remote Turtle files via HTTP
;;;
;;; =============================================================

(load "grab.lsp")

(unless (boundp 'nlt) 
  (load "readers/parse-utils.lsp"))

(defstruct lexer-tape str (bracket-cnt 0) (error nil) 
  (eoe nil)) ;;end-of-expression, where '[' starts array dereference

(defparameter pn_local_breaks (concat lisp_reader_breaks "!$&*+=:/?#@|^<>.")) 
;;characters disallowed in PN_LOCAL

(defparameter varname_breaks (concat pn_local_breaks "-")) 
;;characters disallowed in VARNAME

(defparameter at-id_breaks (concat lisp_reader_breaks "!$&*+=:/?#@|^<>."))
;;characters ending LANGTAG, ('@' is alwayas in beginning)

(defparameter _sq_keywords_ ; SciSPARQL keywords
  '("BASE" "PREFIX" "SELECT" "CONSTRUCT" "DESCRIBE" "ASK" "FROM" "NAMED" ; W3C query types
    "ORDER" "BY" "GROUP" "HAVING" "DESC" "LIMIT" "OFFSET" "DISTINCT" "REDUCED" ; solution modifiers    
    "WHERE" "GRAPH" "OPTIONAL" "UNION" "FILTER" "IN" "NOT" ; conditions
    "TRUE" "FALSE" "AS" "BIND" "EXISTS" "VALUES" "UNDEF"
    "WITH" "DELETE" "DATA" "INSERT" "USING" "ARCHIVE" "TRIPLES" ; W3C updates
    "DEFINE" "FUNCTION" "AGGREGATE" "PROCEDURE" "COST" "FANOUT" "FOR" "QUIT" "LISP")) ; SciSPARQL function declarations and misc.

(defun sparql-stream-lexer-ex (tape)
  (let ((res (sparql-stream-lexer tape)))
    (setf (lexer-tape-eoe tape) 
	  (member (car res) '(right-par right-bracket var)))
    res))

(defun sparql-stream-lexer (tape)
  (do ((chunked-mode (chunked-stream-p (lexer-tape-str tape)))
       (state :general) (token nil) (nextch nil) (buf ""))
      (nil nil)				; never return normally          
    (flet ((read-next () (let ((cc (if chunked-mode (chunked-read-charcode (lexer-tape-str tape))
				     (read-charcode (lexer-tape-str tape)))))
			   (setq nextch (if (eq cc '*eof*) " " (int-char cc)))))
	   (unread () (if chunked-mode (chunked-unread-charcode (char-int nextch) (lexer-tape-str tape))
			(unread-charcode (char-int nextch) (lexer-tape-str tape))))
	   (read-token-g (&optional ws brks strnum nostrings)
	     (if chunked-mode (chunked-read-token (lexer-tape-str tape) ws brks strnum nostrings)
	       (read-token (lexer-tape-str tape) ws brks strnum nostrings))))      
      (setq token (read-token-g nil varname_breaks nil t)) 
      (selectq state
	       (:general 
		 (cond 
		  ((numberp token) (read-next) 
		   (if (string= nextch ".")		       
		       (progn 
			 (read-next) (unread)
			 (return (cons 'number 
				       (concat (mkstring token) "."
					       (if (digit-p nextch) 
						   (mkstring (read-token-g nil nil t))
						 "")))))
		     (progn (unread)
			    (return (cons 'number (mkstring token))))))
		  ((eq token '*eof*) (return nil));;return end-marker
		  ((string= token "^") (read-next) 
		   (if (string= nextch "^") (return '(double-cap))
		     (progn (unread) (return '(cap)))))
		  ((string= token "!") (read-next) 
		   (if (string= nextch "=") (return '(not-equal))
		     (progn (unread) (return '(not)))))
		  ;;process same character in general state
		  ((string= token "|") (read-next) 
		   (if (string= nextch "|") (return '(or))
		     (progn (unread) (return '(pipe)))))
		  ((string= token "&") (read-next) 
		   (if (string= nextch "&") (return '(and))
		     (sq-error "Invalid symbol: &" tape)))			 
		  ((string= token "<") (read-next)
		   (cond
		    ((string= nextch "=") (return '(less-or-equal)))
		    ((or (whitespace-p nextch) (digit-p nextch) 
			 (member nextch '("!" "+" "-" "(" "\"" "'" "?" "$")))
		     ;;precedence of "less"
		     (unread) (return '(less)))
		    ;;process same character in general state  
		    (t (unread)
		       (setq buf (read-token-g ">" "")) ;read until ">"
		       (read-next) (read-next) 
		       (if (string= nextch "(") (return (cons 'uri-cast buf))
			 (progn (unread) (return (cons 'uri buf)))))))
		  ((string= token ">") (read-next) 
		   (if (string= nextch "=") (return '(greater-or-equal))
		     (progn (unread) (return '(greater)))))
		  ;;process same character in general state
		  ((string= token ".") (read-next)
		   ;; nextch is either part of the following number, 
		   ;; or should be processed in general state
		   (cond ((digit-p nextch) ; return the following number prefixed by '.'
			  (unread)
			  (return (cons 'number 
					(concat token (read-token-g)))))
			 ((string= nextch "+") (return '(aplus))) ; return array arithmetic operators
			 ((string= nextch "-") (return '(aminus)))
			 ((string= nextch "*") (return '(atimes)))
			 ((string= nextch "/") (return '(adivide)))
			 ((string= nextch "|") (return '(a-or))) ; return array boolean operators
			 ((string= nextch "&") (return '(a-and)))
			 ((string= nextch "=") (return '(aequal))) ; return array comparison operators			 
			 ((string= nextch "!") 
			  (read-next)
			  (if (string= nextch "=") (return '(anot-equal)) (unread)))
			 ((string= nextch "<")
			  (read-next)
			  (if (string= nextch "=") (return '(aless-or-equal))
			    (progn (unread) (return '(aless)))))
			 ((string= nextch ">")
			  (read-next)
			  (if (string= nextch "=") (return '(agreater-or-equal))
			    (progn (unread) (return '(agreater)))))
			 (t (unread) 
			    (return '(dot))))) ; return simple dot
		  ((string= token "{") (return '(left-brace)))
		  ((string= token "}") (return '(right-brace)))
		  ((string= token "_") (return '(underscore)))
		  ;;not a break character process here only if not part of ID
		  ((string= token ",") (return '(comma)))
		  ((string= token ";") (return '(semicolon)))
		  ((string= token "=") (return '(equal)))
		  ((string= token "+") (return '(plus)))
		  ((string= token "-") (return '(minus)))
		  ((string= token "*") (return '(times)))
		  ((string= token "/") (return '(divide)))
		  ((string= token "(") (return '(left-par)))
		  ((string= token ")") (return '(right-par)))
		  ((string= token "[") 
		   (when (lexer-tape-eoe tape) (incf (lexer-tape-bracket-cnt tape)))
		   (return '(left-bracket)))
		  ((string= token "]") 
		   (when (> (lexer-tape-bracket-cnt tape) 0) 
		     (decf (lexer-tape-bracket-cnt tape)))
		   (return '(right-bracket)))
		  ((string= token ":") 
		   (if (> (lexer-tape-bracket-cnt tape) 0) 
		       (return '(colon))						   
		     (progn (read-next) (unread)
			    (if (whitespace-p nextch) (setq buf "")
			      (setq buf (read-token-g nil pn_local_breaks))) 
			    ;;only allow "_.-%", as in W3C SPARQL 1.1
			    (read-next)
			    (if (string= nextch "(")
				(return (cons 'uri-tail-cast buf))
			      (progn (unread)
				     (return (cons 'uri-tail buf)))))))
		  ((string= token "#") 
		   (read-token-g nl_cr ""))
		  ;;read until the end of line
		  ((string= token "@") 
		   (return (cons 'at-id (read-token-g nil at-id_breaks nil t))))
		  ;;LANGTAG can contain '-' symbol
		  ((string= token "'")
		   ;;read until next single quote, no escapes, 
		   ;; consume the next quote afterwards
		   (let ((res (mkstring (read-token-g "" "'" nil t))))
		     (if (string= res "'") (setq res "")
		       (read-next))
		     (return (cons 'string res))))
		  ((string= token "\"") 
		   (setq nextch token) ;;put back the double quote
		   (unread)		   
		   (return (cons 'string (read-token-g))))
		  ;;read as normal lisp string with escapes
		  ((string= token "$") (setq state :var))
		  ((string= token "?") (read-next) (unread)
		   (if (string-pos varname_breaks nextch) (return '(question))
		     (setq state :var)))
		  ((member token '("?" "$")) (setq state :var))
		  (t (read-next) (unread)
		     (cond ((string= nextch ":")
			    (return (cons 'pref (string-downcase token))))
			   ((member nextch '("-" ".")) ; PREF, since ID is always followed by '('
			    (return (cons 'pref (concat (string-downcase token) nextch 
							(string-downcase (read-token-g nil pn_local_breaks nil t))))))
			   ((string= token "a") 
			    (return '(a)))
			   ((member (string-upcase token) _sq_keywords_)
			    (return (list (mksymbol token))))
;		    ((string= (string-upcase token) "BIND") (return '(bind1))) ;DEBUG		     
			   ((string= (string-upcase token) "EXIT") 
			    (return '(quit)));;alias to QUIT
			   (t (return (cons 'id (string-downcase token))))))))
	       (:var (setq state :general)
		 (return (cons 'var (string-downcase token))))
	       t))))

(defun sq-error (message tape)
  (unless (lexer-tape-error tape)	;unless tape is ALREADY in error state
    (setf (lexer-tape-error tape) t)	;enter error state
    (do ((lexem))
	(nil t)
      (setq lexem (sparql-stream-lexer tape))
      (when (or (null lexem) (equal lexem '(semicolon))) (return t))) 
    ;;advance until next semicolon or *EOF*
    (error message nil)))		;print the message

(defparameter terminals
  '((-! . "<end_of_query>")
    (uri . "<uri>")
    (uri-cast . "<uri>(")
    (var . "?<variable>")
    (id . "<id>")
    (pref . "<prefix>")
    (string . "<string>")
    (number . "<number>")
    (uri-tail . ":<uri_tail>")
    (uri-tail-cast . ":<uri-tail>(")
    (langtag . "@<language_tag>")
    (left-brace . "{")
    (right-brace . "}")
    (underscore . "_")
    (comma . ",")
    (semicolon . ";")
    (equal . "=")
    (plus . "+")
    (minus . "-")
    (times . "*")
    (divide . "/")
    (aequal . ".=")
    (aplus . ".+")
    (aminus . ".-")
    (atimes . ".*")
    (adivide . "./")
    (left-par . "(")
    (right-par . ")")
    (left-bracket . "[")
    (right-bracket . "]")
    (colon . ":")
    (cap . "^")
    (double-cap . "^^")
    (not-equal . "!=")
    (anot-equal . ".!=")
    (not . "!")
    (pipe . "|")
    (or . "||")
    (and . "&&")
    (a-or . ".|")
    (a-and . ".&")
    (less-or-equal . "<=")
    (less . "<")
    (greater-or-equal . ">=")
    (greater . ">")
    (aless-or-equal . ".<=")
    (aless . ".<")
    (agreater-or-equal . ".>=")
    (agreater . ".>")
    (question . "?")
    (dot . ".")))

(defun print-lexems-from-stream (str)
  (do* ((tape (make-lexer-tape :str str))
	(lexem))
      (nil t)
    (setq lexem (sparql-stream-lexer-ex tape))
    (print lexem)
    (unless lexem (return nil))))

(defun print-lexems-from-file (filename)
  (let ((str (openstream filename "r")))
    (unwind-protect
	(print-lexems-from-stream str)
      (closestream str))))

(defun print-lexems (string)
  (with-textstream str string 
		   (print-lexems-from-stream str)))

;;;;;;;;;;;;;;;;;;;;;;; SYNTAX PARSER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grammar is specified in sparql-grammar.lsp

(unless (boundp '_rdf_types_declared_) 
  (load "rdf-types.lsp"))

(load "sparql-lsp-parser-datamodel.lsp")

(load "sparql-slr1.lsp")

(defun syntax-error-msg (res)
  (let ((ts "") t-str)
    (dolist (terminal (third res))
      (unless (string= ts "") (setq ts (concat ts " ")))
      (setq t-str (assoc terminal terminals))
      (setq ts (concat ts (if t-str (cdr t-str) terminal))))
    (setq t-str (assoc (car (fourth res)) terminals))
    (concat "Input: " (if t-str (cdr t-str) (car (fourth res))) 
	    (if (cdr (fourth res)) (concat " = " (cdr (fourth res))) "") " Expected: " ts)))

(defun sparql-parse (stream)
  (let ((tape (make-lexer-tape :str stream))
	(data (make-sparql-data)) res)
    (setq res (sparql-slr1 (f/l () (sparql-stream-lexer-ex tape)) data))
    (if (eq (car res) 'syntax-error) 
	(sq-error (concat "Syntax error: " (syntax-error-msg res)) tape)
      res)))

(defun sparql-parse-string (s)
  (with-textstream stream s (sparql-parse stream)))


; (print-lexems-from-stream (get-url-chunked-stream "http://user.it.uu.se/~andan342/files/temp1.ttl"))

; (let ((cs (get-url-chunked-stream "http://user.it.uu.se/~andan342/files/temp1.ttl"))) (chunked-stream-dub cs) (print-lexems-from-stream cs))

; (let ((cs (get-url-chunked-stream "http://user.it.uu.se/~andan342/files/temp1.ttl1"))) (chunked-stream-dub cs) (print-lexems-from-stream cs))

; turtle(uri( "http://user.it.uu.se/~andan342/files/temp1.ttl"));