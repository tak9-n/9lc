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
   #:node-val
   #:ND_BIGGER
   #:ND_EQ_BIGGER
   #:ND_EQUAL
   #:ND_NOT_EQUAL))

(in-package 9lc.parser)

(defun parse-number (is)
  (let (char-lst)
    (loop
       (let ((c (read-char is nil 'eof)))
         (when (not (find c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
           (unless (eq c 'eof)
             (unread-char c is))
           (return (values (parse-integer (coerce (nreverse char-lst) 'string)) (length char-lst))))
         (push c char-lst)))))

(defun tokenize (is)
  (macrolet ((char-case (&body body)
               (let ((readed-char (gensym)))
                 `(let ((,readed-char (read-char is nil 'eof)))
                    (prog1
                        (case ,readed-char
                          ,@(mapcar (lambda (x)
                                     (if (eq (car x) 't)
                                         `(t ,@(cdr x))
                                         `((,(car x)) ,@(cdr x))))
                                   body))
                        (incf char-cnt))))))
    (do* ((c (read-char is nil 'eof) (read-char is nil 'eof))
          (char-cnt 0 (1+ char-cnt))
          (line-cnt 0 (if (eq c #\Return)
                          (1+ line-cnt)
                          line-cnt))
          tokens)
         ((eq c 'eof) (nreverse tokens))
      (let ((token (case c
                     ((#\  #\Tab) nil)
                     ((#\=)
                      (char-case
                       (#\= `(TK_RESERVED "==" ,char-cnt))
                       (t (error "トークナイズできません(~a)" char-cnt))))
                     ((#\!)
                      (char-case
                       (#\= `(TK_RESERVED "!=" ,char-cnt))
                       (t (error "トークナイズできません(~a)" char-cnt))))
                     ((#\<)
                      (char-case
                       (#\= `(TK_RESERVED "<=" ,char-cnt))
                       (t (prog2
                              (unread-char c is)
                              `(TK_RESERVED "<" ,char-cnt)
                            (decf char-cnt)))))
                     ((#\>)
                      (char-case
                       (#\= `(TK_RESERVED ">=" ,char-cnt))
                       (t (prog2
                              (unread-char c is)
                              `(TK_RESERVED ">" ,char-cnt)
                            (decf char-cnt)))))
                     ((#\+ #\- #\* #\/ #\( #\))
                      `(TK_RESERVED  ,(string c) ,char-cnt))
                     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                      (unread-char c is)
                      (multiple-value-bind (num num-char-cnt)
                          (parse-number is)
                        (prog1
                            `(TK_NUM ,num ,char-cnt)
                          (setf char-cnt (+ char-cnt num-char-cnt)))))
                     (t (error "トークナイズできません(~a)" char-cnt)))))
        (push token tokens)))))

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
              (equal (second token) ,op))
         ,body-reserved-token
         ,body-not-reserved-token)))

(defun consume (anker op) ;todo need to change for string
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

(defun num (anker)
  (make-node :kind 'ND_NUM :val (expect-number anker)))

(defun primary (anker)
  (if (consume anker "(")
      (prog1
          (expr anker)
        (expect anker ")"))
      (num anker)))

(defun unary (anker)
  (cond
    ((consume anker "+")
     (primary anker))
    ((consume anker "-")
     (make-node :kind 'ND_SUB :lhs (make-node :kind 'ND_NUM :val 0) :rhs (primary anker)))
    (t
     (primary anker))))

(defun mul (anker &optional node)
  (let ((left (if node
                  node
                  (unary anker))))
    (cond
      ((consume anker "*")
       (mul anker (make-node :kind 'ND_MUL :lhs left :rhs (unary anker))))
      ((consume anker "/")
       (mul anker (make-node :kind 'ND_DIV :lhs left :rhs (unary anker))))
      (t
       left))))

(defun add-sub (anker &optional node)
  (let ((left (if node
                  node
                  (mul anker))))
    (cond ((consume anker "+")
           (add-sub anker (make-node :kind 'ND_ADD :lhs left :rhs (mul anker))))
          ((consume anker "-")
           (add-sub anker (make-node :kind 'ND_SUB :lhs left :rhs (mul anker))))
          (t left))))

(defun relational (anker &optional node)
  (let ((left (if node
                  node
                  (add-sub anker node))))
    (cond ((consume anker "<")
           (relational anker (make-node :kind 'ND_BIGGER :lhs left :rhs (add-sub anker))))
          ((consume anker "<=")
           (relational anker (make-node :kind 'ND_EQ_BIGGER :lhs left :rhs (add-sub anker))))
          ((consume anker ">")
           (relational anker (make-node :kind 'ND_BIGGER :lhs (add-sub anker) :rhs left)))
          ((consume anker ">=")
           (relational anker (make-node :kind 'ND_EQ_BIGGER :lhs (add-sub anker) :rhs left)))
          (t left))))

(defun equality (anker &optional node)
  (let ((left (if node
                  node
                  (relational anker))))
    (cond ((consume anker "==")
           (equality anker (make-node :kind 'ND_EQUAL :lhs left :rhs (relational anker))))
          ((consume anker "!=")
           (equality anker (make-node :kind 'ND_NOT_EQUAL :lhs left :rhs (relational anker))))
          (t left))))

(defun expr (anker &optional node)
  (equality anker node))

(defun parse (is)
  (let ((tokens (tokenize is)))
    (expr (make-anker :obj tokens))))
