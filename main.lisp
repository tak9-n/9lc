(defpackage 9lc
  (:use common-lisp)
  (:import-from :sb-ext #:*posix-argv*)
  (:export #:cc))

(load "parser.lisp")

(in-package 9lc)

(defun ast2asm-lst (ast)
  (labels ((ast2asm-lst-in (node asm-lst)
             (if (eq (9lc.parser:node-kind node) '9lc.parser:ND_NUM)
                 (cons (format nil "  push ~a~%" (9lc.parser:node-val node)) asm-lst)
                 (progn
                   (setf asm-lst (ast2asm-lst-in (9lc.parser:node-lhs node) asm-lst))
                   (setf asm-lst (ast2asm-lst-in (9lc.parser:node-rhs node) asm-lst))
                   (push (format nil "  pop rdi~%") asm-lst)
                   (push (format nil "  pop rax~%") asm-lst)
                   (push (case (9lc.parser:node-kind node)
                           ((9lc.parser:ND_ADD)
                            (format nil "  add rax, rdi~%"))
                           ((9lc.parser:ND_SUB)
                            (format nil "  sub rax, rdi~%"))
                           ((9lc.parser:ND_MUL)
                            (format nil "  imul rax, rdi~%"))
                           ((9lc.parser:ND_DIV)
                            (format nil "  cqo~%  idiv rdi~%")))
                         asm-lst)
                   (push (format nil "  push rax~%") asm-lst)
                   asm-lst))))
    (nreverse (ast2asm-lst-in ast nil))))
 
(defun to-asm-file(str &optional (stream *standard-output*))
  (with-input-from-string (is str)
    (mapc (lambda (asm)
            (format stream "~a~%" asm))
          (append '(".intel_syntax noprefix"
                    ".global main"
                    "main:")
                  (ast2asm-lst (9lc.parser:parse is))
                  (list (format nil "  pop rax~%  ret~%"))))))

(defun cc (str)
  (uiop:with-temporary-file (:stream os :pathname path :type "s")
    (to-asm-file str os)
    (finish-output os)
    (sb-ext:run-program "/bin/gcc" (list (namestring path)) :output *error-output*)))

(defun run (path)
  (let ((result (sb-ext:run-program path nil)))
    (sb-ext:process-exit-code result)))

(defun try (result source)
  (cc source)
  (let ((ret (run "./a.out")))
    (format t "~a code=~a result=~a~%"
            (if (equal ret result)
                "OK"
                "NG")
            source
            ret)))

(defun test ()
  (try 47 "5+6*7")
  (try 15 "5*(9-6)")
  (try 4 "(3+5)/2"))
