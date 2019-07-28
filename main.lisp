(defpackage 9lc
  (:use common-lisp)
  (:import-from :sb-ext #:*posix-argv*))

(in-package 9lc)

(defun parse-number (is)
  (let (char-lst)
    (loop
       (let ((c (read-char is nil 'eof)))
         (when (not (find c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
           (unless (eq c 'eof)
             (unread-char c is))
           (return (values (parse-integer (coerce char-lst 'string)) (length char-lst))))
         (push c char-lst)))))

(defun tokenize (is)
  (do* ((c (read-char is nil 'eof) (read-char is nil 'eof))
        (char-cnt 0 (1+ char-cnt))
        (line-cnt 0 (if (eq c #\Return)
                        (1+ line-cnt)
                        line-cnt))
        tokens)
       ((eq c 'eof) (nreverse tokens))
    (let ((token (case c
                   ((#\  #\Tab) nil)
                   ((#\+ #\-)
                    `(TK_RESERVED ,c ,char-cnt))
                   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                    (unread-char c is)
                    (multiple-value-bind (num num-char-cnt)
                        (parse-number is)
                      (prog1
                          `(TK_NUM ,num ,char-cnt)
                        (setf char-cnt (+ char-cnt num-char-cnt)))))
                   (t (error "トークナイズできません(~a)" char-cnt)))))
      (push token tokens))))

(defun make-dispenser-object (lst)
  (let ((now-lst lst))
    (lambda ()
      (prog1
       (first now-lst)
       (setf now-lst (rest now-lst))))))

(defun token2asm-lst (token-dispenser)
  (labels ((expect-number ()
             (let ((token (funcall token-dispenser)))
               (if (eq 'TK_NUM (first token))
                   (second token)
                   (error "数字ではありません(~a)" (third token))))))
    (do* ((asm-lst (list (format nil "  mov rax, ~a" (expect-number))))
          (token (funcall token-dispenser) (funcall token-dispenser)))
         ((null token) (nreverse asm-lst))
         (case (first token)
           ('TK_RESERVED
            (push (format nil (if (eq #\+ (second token))
                                  "add rax, ~a"
                                  "sub rax, ~a")
                          (expect-number))
                  asm-lst))
           (t
            (error "expect + or -"))))))

(defun to-asm-file(str &optional (stream *standard-output*))
  (with-input-from-string (is str)
    (mapc (lambda (asm)
            (format stream "~a~%" asm))
          (append '(".intel_syntax noprefix"
                    ".global main"
                    "main:")
                  (token2asm-lst (make-dispenser-object
                                  (tokenize is)))
                  '("  ret")))))

(defun cc(str)
  (uiop:with-temporary-file (:stream os :pathname path :type "s")
    (to-asm-file str os)
    (finish-output os)
    (sb-ext:run-program "/bin/gcc" (list (namestring path)) :output *error-output*)))
