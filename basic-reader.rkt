#lang racket
(provide (rename-out (basic-read-syntax read-syntax)))
(require syntax/readerr)

(define (basic-read-syntax src in)
  (datum->syntax
   #f
   `(module basic racket
      (require "basic.rkt")
      (basic
       ,@(read-program src in)))))

(define (next-token src in (peek? #f))
  (skip-whitespace in)
  (define match (if peek? regexp-match-peek regexp-try-match))
  (cond
    ((match #rx"^(PRINT|GOTO|GOSUB|RETURN|IF|THEN|ELSE|\\*|\\+|-|/|=|<|>|<=|>=)" in)
     => (lambda (match)
          (string->symbol (bytes->string/utf-8 (car match)))))
    ((match #rx"^\\(" in)
     'open-paren)
    ((match #rx"^\\)" in)
     'closed-paren)
    ((match #rx"^," in)
     'comma)
    ((match #rx"^[0-9]+" in)
     => (lambda (match)
          (string->number (bytes->string/utf-8 (car match)))))     
    ((match #rx"^[a-zA-Z]+$?" in)
     => (lambda (match)
          (string->symbol (bytes->string/utf-8 (car match)))))
    ((match #rx"\"([^\"]+)\"" in)
     => (lambda (match)
          (bytes->string/utf-8 (cadr match))))
    ((eof-object? (peek-char in))
     eof)
    ((equal? #\newline (peek-char in))
     (read-char in)
     eof)
    ((match "^$" in)
     eof)
    (else
     (complain src in "unknown lexeme"))))

(define (t)
  (define p (open-input-string "foo bar baz 10"))
  (define t1 (next-token #f p 'peek))
  (define t2 (next-token #f p))
  (define t3 (next-token #f p))
  (values t1 t2 t3))
  

(define (tokenize src in)
  (define token (next-token src in))
  (if (eof-object? token)
      '()
      (cons token (tokenize src in))))

(define (get-line-number src in)
  (regexp-try-match #px"^\\s+" in)
  (cond
    ((regexp-try-match #rx"^[0-9]+" in)
     => (lambda (match)
          (string->number (bytes->string/utf-8 (car match)))))
    (else
     (complain src in "no line number"))))

(define (complain src in msg)
  (define-values (line col pos) (port-next-location in))
  (raise-read-error msg src line col pos 1))

(define (parse-line src in)
  (regexp-try-match #px"^\\s+" in)
  (if (eof-object? (peek-char in))
      eof
      (let ()
        (define line-number (get-line-number src in))
        (define command (parse-command src in))
        `(,line-number ,command))))

(define (parse-command src in)
  (define first-token (next-token src in))
  (when (eof-object? first-token)
    (error "no command after line number"))
  (cond
    ((eq? 'PRINT first-token)
     `(print ,@(parse-arguments src in)))
    ((eq? 'GOTO first-token) `(goto ,(get-line-number src in)))
    ((eq? 'GOSUB first-token) `(gosub ,(get-line-number src in)))
    ((eq? 'RETURN first-token) '(return))
    ((eq? 'IF first-token)
     (define test (parse-expr src in))
     (unless (eq? 'THEN (next-token src in))
       (complain src in "missing THEN in IF"))
     (define then (parse-command src in))
     (unless (eq? 'ELSE (next-token src in))
       (complain src in "missing ELSE in IF"))
     (define else (parse-command src in))
     `(if ,test ,then ,else))
    ((symbol? first-token)
     (unless (eq? '= (next-token src in))
       (complain src in "incomplete assignment"))
     (define expr (parse-expr src in))
     `(:= ,first-token ,expr))))

(define (parse-arguments src in)
  (define first (parse-expr src in))
  (if (eq? 'comma (next-token src in 'peek))
      (begin
        (next-token src in)
        (cons first (parse-arguments src in)))
      (list first)))

(define (skip-whitespace in)
  (regexp-try-match #px"^[ \t]+" in))

(define (parse-expr src in)
  (define left (parse-expr-1 src in))
  (define next (next-token src in 'peek))
  (cond
    ((eof-object? next) left)
    ((memq next '(= < > <= >=))
     (next-token src in)
     (define right (parse-expr src in))
     `(,next ,left ,right))
    (else
     left)))

(define (parse-expr-1 src in)
  (define left (parse-expr-2 src in))
  (define next (next-token src in 'peek))
  (cond
    ((eof-object? next) left)
    ((memq next '(+ -))
     (next-token src in)
     (define right (parse-expr-1 src in))
     `(,next ,left ,right))
    (else
     left)))

(define (parse-expr-2 src in)
  (define left (parse-expr-3 src in))
  (define next (next-token src in 'peek))
  (cond
    ((eof-object? next) left)
    ((memq next '(* /))
     (next-token src in)
     (define right (parse-expr-2 src in))
     `(,next ,left ,right))
    (else
     left)))

(define (parse-expr-3 src in)
  (define next (next-token src in))
  (cond
    ((eof-object? next)
     (complain src in "premature end of input"))
    ((eq? next 'open-paren)
     (define expr (parse-expr src in))
     (define after (next-token src in))
     (unless (eq? 'closed-paren after)
       (complain src in "no closing parenthesis"))
     expr)
    (else next)))

(define (read-program src in)
  (define line (parse-line src in))
  (if (eof-object? line)
      '()
      (cons line (read-program src in))))
