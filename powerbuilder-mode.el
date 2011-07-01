;; the command to comment/uncomment text
(defun powerbuilder-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "// ") (comment-end ""))
    (comment-dwim arg)))

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq pb-font-lock-keywords
      (let ((pb-keywords
	     '("alias" "and" "autoinstantiate" "call" "case"
	       "catch" "choose" "close" "commit" "connect"
	       "constant" "continue" "create" "cursor"
	       "declare" "delete" "describe" "descriptor"
	       "destroy" "disconnect" "do" "dynamic" "else"
	       "elseif" "end" "enumerated" "event" "execute"
	       "exit" "external" "false" "fetch" "finally"
	       "first" "for" "forward" "from" "function"
	       "global" "goto" "halt" "if" "immediate"
	       "indirect" "insert" "into" "intrinsic" "is"
	       "last" "library" "loop" "native" "next" "not"
	       "of" "on" "open" "or" "parent" "post"
	       "prepare" "prior" "private" "privateread"
	       "privatewrite" "procedure" "protected"
	       "protectedread" "protectedwrite" "prototypes"
	       "public" "readonly" "ref" "return" "rollback"
	       "rpcfunc" "select" "selectblob" "set"
	       "shared" "static" "step" "subroutine" "super"
	       "system" "systemread" "systemwrite" "then"
	       "this" "throw" "throws" "to" "trigger" "true"
	       "try" "type" "until" "update" "updateblob"
	       "using" "variables" "where" "while" "with"
	       "within " "xor " "_debug"))
	    (pb-types
	     '("this" "super" "parent" "blob" "integer"
	       "int" "boolean" "longlong" "byte" "long"
	       "char" "character" "real" "date" "string"
	       "datetime" "time" "decimal" "dec"
	       "unsignedinteger" "unsignedint" "uint"
	       "double" "unsignedlong" "ulong"))
	    (pb-constants
	     '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
	    (pb-events
	     '("at_rot_target" "at_target" "attach"))
	    (pb-functions
	     '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList")))
	
	;; note: order below matters.
	`((,(regexp-opt pb-types 'words)     . font-lock-type-face)
	  (,(regexp-opt pb-constants 'words) . font-lock-constant-face)
	  (" *\\w+:\\|goto +\\w+" . font-lock-reference-face)
	  (,(regexp-opt pb-constants 'words) . font-lock-builtin-face)
	  (,(regexp-opt pb-functions 'words) . font-lock-function-name-face)
	  (,(regexp-opt pb-keywords 'words)  . font-lock-keyword-face)
	  )))

(defun powerbuilder-match-line (matchers)
  (let ((continue (not (bobp)))
        (result 'not-found)
        (rest matchers))
    (while (and continue (setq head (car rest)))
      (if (looking-at-p (elt head 0))
          (progn
            (setq continue nil)
            (setq result (elt head 1)))
        (setq rest (cdr rest))))
    
    result))

(defun powerbuilder-search-back-for (matchers)
  (let ((continue (not (bobp)))
        (result 'not-found))  
    
    (while continue
      (forward-line -1)

      (setq result (powerbuilder-match-line matchers))

      ;; Stop if we found a match
      (if (not (eq result 'not-found))
          (setq continue nil))

      ;; Stop if we are at the begining of the buffer
      (if (bobp)
          (setq continue nil)))
    
    result))   

(defun powerbuilder-indent-on (matchers)
  (save-excursion
    (let ((match
           (powerbuilder-search-back-for matchers)))

      (if (eq 'not-found match) 
          0 
        (+ (current-indentation) match)))))


(setq powerbuilder-for-regexp "^[ \t]*for[ \t]+.*[ \t]+to[ \t]+")
(setq powerbuilder-next-regexp "^[ \t]*next")

(setq powerbuilder-if-regexp "^[ \t]*if.*\\(then\\|&\\)[ \t]*$")
(setq powerbuilder-else-regexp "^[ \t]*else[ \t]*$")
(setq powerbuilder-end-if-regexp "^[ \t]*end if")

(setq powerbuilder-do-while-regexp "^[ \t]*do[ \t]+while")
(setq powerbuilder-loop-regexp "^[ \t]*loop")

(setq powerbuilder-function-regexp "^[ \t]*\\(public\\|protected\\|private\\)[ \t]+function[ \t]+.+)[ \t]*;")
(setq powerbuilder-end-function-regexp "^[ \t]*end[ \t]+function")

(setq powerbuilder-choose-regexp "^[ \t]*choose[ \t]+case[ \t]+")
(setq powerbuilder-end-choose-regexp "^[ \t]*end[ \t]+choose")
(setq powerbuilder-case-regexp "^[ \t]*case[ \t]+")

(setq powerbuilder-type-from-regexp "^[ \t]*\\(global[ \t]+\\)?type[ \t]+.+[ \t]+from[ \t]+")
(setq powerbuilder-end-type-regexp "^[ \t]*end[ \t]+type")

(setq powerbuilder-event-regexp "^[ \t]*event[ \t]+")
(setq powerbuilder-end-event-regexp "^[ \t]*end[ \t]+event")

(setq powerbuilder-on-regexp "^[ \t]*on[ \t]+")
(setq powerbuilder-end-on-regexp "^[ \t]*end[ \t]+on")

(setq powerbuilder-forward-prototypes-regexp "^[ \t]*forward[ \t]+prototypes")
(setq powerbuilder-end-prototypes-regexp "^[ \t]*end[ \t]+prototypes")

(setq powerbuilder-forward-regexp "^[ \t]*forward")
(setq powerbuilder-end-forward-regexp "^[ \t]*end[ \t]+forward")

(setq powerbuilder-type-variables-regexp "^[ \t]*type[ \t]+variables")
(setq powerbuilder-end-variables-regexp "^[ \t]*end[ \t]+variables")


(setq indenters 
      `(,powerbuilder-for-regexp
        ,powerbuilder-if-regexp
        ,powerbuilder-do-while-regexp
        ,powerbuilder-function-regexp
        ,powerbuilder-choose-regexp
        ,powerbuilder-type-from-regexp
        ,powerbuilder-event-regexp
        ,powerbuilder-on-regexp
        ,powerbuilder-forward-prototypes-regexp
        ,powerbuilder-forward-regexp
        ,powerbuilder-type-variables-regexp))

(setq line-indenters 
      `(,powerbuilder-case-regexp
        ,powerbuilder-else-regexp))

(setq unindenters 
      `(,powerbuilder-loop-regexp
        ,powerbuilder-next-regexp
        ,powerbuilder-end-if-regexp
        ,powerbuilder-end-function-regexp
        ,powerbuilder-end-choose-regexp
        ,powerbuilder-end-type-regexp
        ,powerbuilder-end-event-regexp
        ,powerbuilder-end-on-regexp
        ,powerbuilder-end-prototypes-regexp
        ,powerbuilder-end-forward-regexp
        ,powerbuilder-end-variables-regexp))

(setq powerbuilder-end-block-indent-rules 
      `((,powerbuilder-end-if-regexp 
         ((,powerbuilder-if-regexp 0)
          (,powerbuilder-else-regexp 0)))

        (,powerbuilder-else-regexp
         ((,powerbuilder-if-regexp 0)
          (,powerbuilder-else-regexp 0)))
        
        (,powerbuilder-next-regexp
         ((,powerbuilder-for-regexp 0)))

        (,powerbuilder-loop-regexp
         ((,powerbuilder-do-while-regexp 0)))

        (,powerbuilder-end-function-regexp
         ((,powerbuilder-function-regexp 0)))

        (,powerbuilder-end-choose-regexp
         ((,powerbuilder-choose-regexp 0)))

        (,powerbuilder-case-regexp
         ((,powerbuilder-choose-regexp 2)
          (,powerbuilder-case-regexp 0)))

        (,powerbuilder-end-type-regexp
         ((,powerbuilder-type-from-regexp 0)))

        (,powerbuilder-end-event-regexp
         ((,powerbuilder-event-regexp 0)))

        (,powerbuilder-end-on-regexp
         ((,powerbuilder-on-regexp 0)))

        (,powerbuilder-end-prototypes-regexp
         ((,powerbuilder-forward-prototypes-regexp 0)))
        
        (,powerbuilder-end-forward-regexp
         ((,powerbuilder-forward-regexp 0)))
        
        (,powerbuilder-end-variables-regexp         
         ((,powerbuilder-type-variables-regexp 0)))
        ))

(defun powerbuilder-regexps-with-indent (indent-level regexps)
  (map 'list (lambda (regexp) `(,regexp ,indent-level)) regexps))

(defun powerbuilder-remove-all (elements-to-be-removed seq) 
  (reduce (lambda (result element) (remove* element result)) 
          elements-to-be-removed :initial-value seq))

(defun powerbuilder-indent-before ()
  (powerbuilder-indent-on
   (append 
    (powerbuilder-regexps-with-indent 2 indenters)
    (powerbuilder-regexps-with-indent 2 line-indenters)
    (powerbuilder-regexps-with-indent 0 unindenters))))

(defun powerbuilder-proper-indentation () 
  (let ((match 
         (powerbuilder-maching-line 
          powerbuilder-end-block-indent-rules)))
    
    (cond 
     ((not (eq match 'not-found))
      (powerbuilder-indent-on
       (append 
        match
        (powerbuilder-regexps-with-indent -2 unindenters))))
     
     ('else (powerbuilder-indent-before)))))


(defun powerbuilder-indent-line () 
  "Indent the current line as Powerscript source text."
  (interactive)
  
  (beginning-of-line)
  (indent-line-to (max 0 (powerbuilder-proper-indentation))))

;; define the mode
(define-derived-mode powerbuilder-mode fundamental-mode
  "powerbuilder mode
Major mode for editing Powerbuilder script"


  ;; code for syntax highlighting
  (setq font-lock-defaults 
	'(pb-font-lock-keywords 
	  nil ; Syntactic fontification should be performed
	  t   ; Case insensitive
	  ))

  
  ;; c style comment “// …” and “/* … */”
  (modify-syntax-entry ?\/ ". 124b" powerbuilder-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" powerbuilder-mode-syntax-table) 
  (modify-syntax-entry ?* ". 23" powerbuilder-mode-syntax-table)
  ;; Strings
  (modify-syntax-entry ?\\ "." powerbuilder-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" powerbuilder-mode-syntax-table)
  ;; Symbols 
  (modify-syntax-entry ?_ "w" powerbuilder-mode-syntax-table)

  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-statt-skip) "\\(//+\\|/\\*+\\)\\s *")
  (set (make-local-variable 'comment-end) " */")

  (set (make-local-variable 'indent-line-function) 'powerbuilder-indent-line)

  ;; modify the keymap
  (define-key powerbuilder-mode-map [remap comment-dwim] 'powerbuilder-comment-dwim)
  )

(provide 'powerbuilder-mode)