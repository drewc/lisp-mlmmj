(defpackage :drewc.org/lisp-mlmmj/mailman->mlmmj
  (:use :cl )
  (:import-from :drewc.org/smug/package)
  (:export 
  ))

(in-package :drewc.org/lisp-mlmmj/mailman->mlmmj)

(defun p-split (sep)
  (smug:let* ((first (smug:every
		      (smug:is-not 'eql sep)))
	      (rest (smug:maybe
		     (smug:progn 
		       (smug:is 'eql sep)
		       (p-split sep))
		     (smug:result nil))))
    (smug:result (cons 
		  (coerce first 'cl:string)
		  rest))))

(defun until (char)
  (smug:some (smug:is-not #'eql char)))

(defun pickle-string (&optional (quote #\'))
  (smug:let* ((string 
	       (smug:prog2 
		   (smug:is #'char= quote)
		   (smug:some (smug:is-not #'char= quote))
		 (smug:is #'char= quote))))
    (smug:result (coerce string 'cl:string))))

(defun pickle-number ()
  (smug:every (smug:or  (smug:is #'digit-char-p)
			(smug:is #'char= #\.))))

(defun pickle-bool ()
  (smug:or  (smug:string "True")
	    (smug:string "False")
	    (smug:string "None")))

(defun pickle-empty-alist ()
  (smug:prog2 
      (smug:is 'eql #\{)
      (smug:some 
       (smug:is 'eql #\Space))
    (smug:is 'eql #\})))

(defun pickle-empty-list ()
  (smug:prog2 
      (smug:is 'eql #\[)
      (smug:some 
       (smug:is 'eql #\Space))
    (smug:is 'eql #\])))


(defun pickle-list-string ()
  (smug:progn
    (until #\')
    (pickle-string)))

(defun pickle-list ()
  (smug:let* ((foo (smug:prog2
		       (smug:is 'eql #\[)
		       (smug:every (smug:is-not 'eql #\]))
		     (smug:is 'eql #\]))))
    (smug:result (coerce foo 'string))))

(defun pickle-alist-pair ()
  (smug:progn
    (until #\')
    (pickle-pair)))

(defun pickle-alist ()

  (smug:prog2 
        (smug:is 'eql #\{)
      (smug:let* ((x (pickle-alist-pair))
		  (xs (smug:every
		       (smug:prog2 
			 (smug:is 'eql #\,)
			   (pickle-alist-pair))))
		 )
	(smug:result (cons x xs))
	  ))
  )

(defun pickle-value ()
  (smug:progn
    (smug:not (smug:is 'eql #\,))
    (smug:or 
     (pickle-list)
     (pickle-empty-list)
     (pickle-number)
     (pickle-string)
     (pickle-string #\")
     (smug:progn 
       (smug:is 'eql #\u)
       (pickle-string))
     (pickle-empty-alist)
     (pickle-bool)
     (smug:prog1  
	 (pickle-alist)
       (smug:is 'eql #\})))))

(defun pickle-pair ()
  "A pair is STRING: VALUE"
  (smug:let* ((car (smug:prog1
		       (pickle-string)
		     (smug:string ": ")))
	      (cdr (pickle-value)))
    (smug:result (cons car cdr))))

(defun parse-pickle ()
  (smug:progn 
    (until #\{)
    (smug:prog1  
	 (pickle-alist)
       (smug:is 'eql #\}))))


