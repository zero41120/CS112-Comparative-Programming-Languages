#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; AUTHOR
;;    Tz Shiuan Lin
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; ========= Provided codes =========
(define *stderr* (current-error-port))
    
(define *run-file*
  (let-values
    (((dirpath basepath root?)
      (split-path (find-system-path 'run-file))))
    (path->string basepath))
  )

(define (die list)
  (for-each (lambda (item) (display item *stderr*)) list)
  (newline *stderr*)
  (exit 1)
  )

(define (usage-exit)
  (die `("Usage: " ,*run-file* " filename"))
  )

(define (readlist-from-inputfile filename)
  (let ((inputfile (open-input-file filename)))
   (if (not (input-port? inputfile))
     (die `(,*run-file* ": " ,filename ": open failed"))
     (let ((program (read inputfile)))
      (close-input-port inputfile)
      program)))
  )

(define (write-program-by-line filename program)
  (printf "==================================================~n")
  (printf "~a: ~s~n" *run-file* filename)
  (printf "==================================================~n")
  (printf "(~n")
  (map (lambda (line) (printf "~s~n" line)) program)
  (printf ")~n"))


;; ========= Data Tables Implementations =========
;; Set DEBUG_MODE to 1 to print debug statements
(define DEBUG_MODE 0)
(define (print_debug string) 
  (cond ((equal? DEBUG_MODE 1) 
    (display "    ")(display string)(newline)
    )
  )
)

;; Create the reference tables
(define *function-table* (make-hash))
(define *expression-table* (make-hash))
(define *lable-table* (make-hash))
(define *variable-table* (make-hash))

;; Create tables' Getter and setter
(define (function-get   key) (hash-ref *function-table* key))
(define (expression-get key) (hash-ref *expression-table* key))
(define (lable-nr-get   key) (hash-ref *lable-table* key))
(define (variable-get   key) (hash-ref *variable-table* key))
(define (function-set!   key value) 
  (hash-set! *function-table* key value))
(define (expression-set! key value) 
  (hash-set! *expression-table* key value))
(define (lable-nr-set!   key value) 
  (hash-set! *lable-table* key value))
(define (variable-set!   key value) 
  (hash-set! *variable-table* key value))

;; Insert function table. Most functions provided by Mackey
(for-each
  (lambda (pair)
    (function-set! (car pair) (cadr pair)))
  `(
        ;; number
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (inputcount -1)
        ;; operators
        (+       ,+)  (-       ,-)  (*       ,*)
        (>       ,>)  (<       ,<)
        (<=      ,<=) (>=      ,>=) 
        ;; builtin symbols
        (^       ,expt)
        (abs     ,abs) (acos    ,acos) (asin    ,asin) 
        (atan    ,atan) (ceil    ,ceiling) (cos     ,cos)
        (exp     ,exp) (floor   ,floor) 
        (round   ,round) (sin     ,sin) (sqrt    ,sqrt)
        (tan     ,tan) (trunc   ,truncate)
        ;; function that needs special insturction
        (=       ,(lambda (x y) 
          (print_debug (equal? (+ x 0.0) (+ y 0.0))) 
          (equal? (+ x 0.0) (+ y 0.0)))
        )
        (<>      ,(lambda (x y) (not (equal? (+ x 0.0) (+ y 0.0)))))
        (log     ,(lambda (x) (if (equal? x 0) null (log x))))
        (/       ,(lambda (x y) (/ (+ x 0.0)(+ y 0.0))))
        (div     ,(lambda (x y) (floor (/ x y))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
    )
)

;; insert label table
(define (hash-labels program)
  (map (lambda (line) 
    (when (not (null? line))
      (when (or 
            (= 3 (length line))
            (and (= 2 (length line)) 
            (not (list? (cadr line))))
            )
        (lable-nr-set! (cadr line) (- (car line) 1 ))
      )
    )
    ) 
  program
  )
)

;; ========= BASIC interpreter Implementations =========
;; evaluate command
(define (evaluate-cmd cmd)
  (print_debug "-evaluate-cmd: ")
  (print_debug cmd)
  (cond 
    ((string? cmd) (print_debug "string") cmd)
    ((number? cmd) (print_debug "number") cmd)
    ((hash-has-key? *variable-table* cmd)
      (print_debug "variable")
      (print_debug (variable-get cmd))
      (if (list? (variable-get cmd))
        (car (variable-get cmd))
        (variable-get cmd)
      )
    )
    ((hash-has-key? *function-table* cmd) 
      (print_debug "function")
      (function-get cmd)
      )
    ((list? cmd)
      (if (hash-has-key? *function-table* (car cmd))
            ;; ture
            (let ((first (function-get (car cmd))))
              (cond 
                ((procedure? first)
                  (print_debug "list_procedure")
                  (apply first (map 
                    (lambda (x) (evaluate-cmd x)) (cdr cmd)))
                  )
                ((vector? first)
                  (print_debug "list_vector")
                  (vector-ref first (cadr cmd)))
                ((number? first) (print_debug "list_number") first)
                (else (die "function table error"))
                )
              )
            ;; false
            (die (list "Command: " (car cmd) " not found.\n"))
          ) ;; end if
        );; list
    (else 
      (print_debug "junk?")
      )
    );; cond
);; define

;; evaluate line
(define (evaluate-line program line-nr) 
  (when (> (length program) line-nr)
    (let((line (list-ref program line-nr)))
      (cond
        ;; line == 3
        ((= (length line) 3)
          (set! line (cddr line))
          (execute (car line) program line-nr)
          )
        ;; line == 2 && list
        ((and (= (length line) 2) (list? (cadr line)))
          (set! line (cdr line))
          (execute (car line) program line-nr)
          )
        ;; next
        (else 
          (evaluate-line program (+ line-nr 1))
          )
      );; cond
      )
    )
  )

;; execute
(define (execute expr program line-nr)
  (print_debug "-execute :")
  (print_debug expr)

  (when (not (hash-has-key? *expression-table* (car expr)))
    (die "No such expression" (car expr)))
  (cond
    ;; goto
    ((eq? (car expr) 'goto)
      (evaluate-line program (lable-nr-get (cadr expr))))
    ;; if
    ((eq? (car expr) 'if)
      (print_debug "if")
      (if (evaluate-cmd (cadr expr))
        (evaluate-line program (lable-nr-get (cadr (cdr expr))))
        (evaluate-line program (+ line-nr 1))
        )
      )
    ;; print
    ((eq? (car expr) 'print)
      (if (null? (cdr expr))
        (newline)
        (do_print (cdr expr)))
      (evaluate-line program (+ line-nr 1)))
    ;; else    
    (else
      (print_debug "expression table search")
      ((expression-get (car expr)) (cdr expr))
      (evaluate-line program (+ line-nr 1))
      )
    )
  )


;; Statements 
;; 1. dim: array declaration 
(define (do_dim name)
  (print_debug "-do_dim")
  (set! name (car name))
  (let ((arr (make-vector (evaluate-cmd (cadr name))(car name))))
    (variable-set! (car name)(+ (evaluate-cmd (cadr name)) 1))
    )
)
;; 2. let: variable assignment
(define (do_let name)
  (print_debug "-do_let")
  (variable-set! (car name) (evaluate-cmd (cadr name)))
  (print_debug "end_do_let")
)
;; 3. goto: program branch
(define (do_goto lable)
  (print_debug "-do_goto")
  ;; included in th execute procedure
)
;; 4. if
(define (do_if relop lable)
  (print_debug "-do_if")
  ;; included in th execute procedure
)
;; 5. print: display
(define (do_print printable)
  (print_debug "-do_print")
  (print_debug printable)
  (map (lambda (x) (display (evaluate-cmd x))) printable)
  (newline)
  (print_debug "end_do_print")
  )
;; 6. input
(define (save key)
    (define inputs (list (read)))
    (variable-set! key inputs)
    (print_debug key)
    (print_debug "saved:")
    (print_debug (variable-get key))
)
(define (do_input memory)
  (print_debug "-do_input")
  (print_debug memory)
  (if (> 1 (length memory))
    (display "a list, no time to do this part")
    (save (car memory))
  )
)

;; insert expression table
(for-each
  (lambda (pair)
    (expression-set! (car pair) (cadr pair)))
  `(
        ;; expression
        (dim   ,do_dim) 
        (let   ,do_let) 
        (goto  ,do_goto)
        (if    ,do_if) 
        (print ,do_print) 
        (input ,do_input)
        )
  )

;; Main
(define (main arglist)
  (print_debug "program start")
  (if (or (null? arglist) (not (null? (cdr arglist))))
    (usage-exit)
    (let* ((sbprogfile (car arglist))
      (program (readlist-from-inputfile sbprogfile)))
      (hash-labels program)
      (evaluate-line program 0)
      ;(write-program-by-line sbprogfile program)
    )
  )
  (print_debug "program end")
)

(main (vector->list (current-command-line-arguments)))
