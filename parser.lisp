(defpackage 9lc.parser
  (:use common-lisp)
  (:export
   #:ND_NUM
   #:ND_MUL
   #:ND_DIV
   #:ND_ADD
   #:ND_SUB
   #:parse
   #:node
   #:make-node
   #:node-p
   #:copy-node
   #:node-kind
   #:node-lhs
   #:node-rhs
   #:node-val))

(in-package 9lc.parser)

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
                   ((#\+ #\- #\* #\/ #\( #\))
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

(defstruct anker
  obj)

(defstruct node
  kind
  lhs
  rhs
  val)

;; 罪深いマクロの使い方をしているので、ちゃんと書き直さないとまずい。。。
(defmacro with-token (anker &body body)
  `(let* ((tokens (anker-obj ,anker))
          (token (first tokens)))
     ,@body))

(defmacro if-reserved-token (anker op body-reserved-token body-not-reserved-token)
  `(with-token ,anker
     (if (and (eq (first token) 'TK_RESERVED)
              (eq (second token) ,op))
         ,body-reserved-token
         ,body-not-reserved-token)))

(defun consume (anker op)
  (if-reserved-token anker op
    (progn
      (setf (anker-obj anker) (rest tokens))
      t)
    nil))

(defun expect (anker op)
  (if-reserved-token anker op
                     (setf (anker-obj anker) (rest tokens))
                     (error "~a ではありません~%" op)))

(defun expect-number (anker)
  (with-token anker
    (if (eq (first token) 'TK_NUM)
        (progn
          (setf (anker-obj anker) (rest tokens))
          (nth 1 token))
        (error "数ではありません~%"))))

(defun term (anker)
  (if (consume anker #\()
      (prog1
          (expr anker)
        (expect anker #\)))
      (make-node :kind 'ND_NUM :val (expect-number anker))))

(defun mul (anker &optional node)
  (let ((node (if node
                  node
                  (term anker))))
    (cond
      ((consume anker #\*)
       (mul anker (make-node :kind 'ND_MUL :lhs node :rhs (term anker))))
      ((consume anker #\/)
       (mul anker (make-node :kind 'ND_DIV :lhs node :rhs (term anker))))
      (t
       node))))

(defun expr (anker &optional node)
  (let ((node (if node
                  node
                  (mul anker))))
    (cond
      ((consume anker #\+)
       (expr anker (make-node :kind 'ND_ADD :lhs node :rhs (mul anker))))
      ((consume anker #\-)
       (expr anker (make-node :kind 'ND_SUB :lhs node :rhs (mul anker))))
      (t
       node))))

(defun parse (is)
  (let ((tokens (tokenize is)))
    (expr (make-anker :obj tokens))))
