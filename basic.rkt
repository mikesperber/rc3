#lang racket
(provide (all-from-out racket)
         basic print goto :=  gosub return)
(require (for-syntax syntax/parse
                     racket/syntax
                     syntax/id-set))

#|
(print "1+1=" (+ 1 1))
=>
(begin
  (display "1+1=")
  (display (+ 1 1)))
|#

(define-syntax (print form)
  (syntax-parse form
    ((print arg:expr ...)
     #`(begin
         (display arg) ...
         (newline)))))

#|
10 PRINT "Hello, world!"
20 GOTO 10
|#

#|
(10 (print "Hello, world!"))
(20 (goto 10))

=>
(define (line-10)
  (print "Hello, world!")
  (line-20))
(define (line-20)
  (line-10))
|#

(define-for-syntax (make-line-name context line-number)
  (format-id context "line-~a" (syntax-e line-number)))

(define-syntax (basic form)
  (syntax-parse form
    ((basic (line-number:integer command:expr) ...)
     (define id-set (mutable-free-id-set))
     (for-each (lambda (command)
                 (collect-variables command id-set))
               (syntax->list #`(command ...)))
     #`(begin
         #,@(map (lambda (variable)
                   #`(define #,variable #f))
                 (free-id-set->list id-set))
         #,@(map (lambda (line-number next-line-number command)
                   (define name (make-line-name #`basic line-number))
                   (define call-next-line
                     (if next-line-number
                         #`(#,(make-line-name #`basic next-line-number))
                         #`(void)))
                   #`(define (#,name)
                       #,(translate-command #`basic command call-next-line)))
                 (syntax->list #`(line-number ...))
                 (append (cdr (syntax->list #`(line-number ...))) '(#f))
                 (syntax->list #`(command ...)))))))

(define goto #f)
(define := #f)
(define gosub #f)
(define return #f)

(define-for-syntax (translate-command context command call-next-line)
  (syntax-parse command
    #:literals (goto := if gosub return)
    ((goto line-number:integer)
     #`(#,(make-line-name context #`line-number)))
    ((gosub line-number:integer)
     #`(begin
         (#,(make-line-name context #`line-number))
         #,call-next-line))
    ((return) (void))
    ((:= variable:id rhs:expr)
     #`(begin
         (set! variable rhs)
         #,call-next-line))
    ((if test:expr then:expr else:expr)
     #`(if test
           #,(translate-command context #`then call-next-line)
           #,(translate-command context #`else call-next-line)))
    (_
     #`(begin #,command
              #,call-next-line))))

(define-for-syntax (collect-variables command id-set)
  (syntax-parse command
    #:literals (:= if)
    ((:= variable:id rhs:expr)
     (free-id-set-add! id-set #`variable))
    ((if test:expr then:expr else:expr)
     (collect-variables #`then id-set)
     (collect-variables #`else id-set))
    (_ (void))))
    

(basic
 (5 (:= a 42)) ; A = 42
 (7 (if (= a 42) (:= b 1) (:= b 2)))
 (8 (gosub 1000))
 (10 (print "Hello, world " a " " b))
 (20 (goto 40))
 (30 (print "Hello again"))
 (40 (print "The end"))
 (1000 (print b))
 (1010 (return)))
