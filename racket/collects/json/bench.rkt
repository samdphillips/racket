#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/cmdline
         racket/file
         racket/port
         racket/string
         racket/system
         racket/treelist

         (prefix-in rkt: json)
         (prefix-in exp:
                    (combine-in
                     json/exp
                     (submod json/exp for-extension))))

(define-syntax define-opt
  (lambda (stx)
    (syntax-case stx ()
      [(_ name default)
       (with-syntax ([defname (format-id #'name "opt-~a" #'name #:subs? #t)])
         #'(define (defname opt) (hash-ref opt 'name default)))])))

(define-opt copies 1)
(define-opt iters 1)
(define-opt source-file #f)

(define (make-read-all name rd)
  (procedure-rename
   (lambda (in)
     (let rec ()
       (unless (eof-object? (rd in)) (rec))))
   name))

(define (extended-reader in)
  (exp:read-json* 'ext:read-json
                  in
                  (exp:json-null)
                  string->immutable-string
                  list->treelist))

(define (run)
  (define opts
    (let ([h (make-hash)])
      (command-line
       #:once-each
       ["-c" copies "number of copies of the input to process (default: 1)"
             (hash-set! h 'copies (string->number copies))]
       ["-i" iters "number of times to collect timing (default: 1)"
             (hash-set! h 'iters (string->number iters))]
       #:args (source-file)
       (let ([h (hash-map/copy h values #:kind 'immutable)])
         (hash-set h 'source-file (string->path source-file))))))
  (define input-bytes
    (let ([orig (file->bytes (opt-source-file opts))])
      (call-with-output-bytes
       (lambda (out)
         (for ([n (opt-copies opts)])
           (write-bytes orig out)
           (write-bytes #"\n" out))))))
  (define rev
    (string-trim
     (with-output-to-string
       (lambda ()
         (system* (find-executable-path "git") "rev-parse" "HEAD")))))
  (define size (bytes-length input-bytes))
  (define cur-t (current-seconds))
  (for* ([rd (list (make-read-all 'rkt rkt:read-json)
                   (make-read-all 'exp exp:read-json)
                   (make-read-all 'ext extended-reader))]
         [i (opt-iters opts)])
    (define-values (res t-cpu t-real t-gc)
      (call-with-input-bytes input-bytes
                             (lambda (in)
                               (collect-garbage)
                               (collect-garbage)
                               (collect-garbage)
                               (time-apply rd (list in)))))
    (writeln (list cur-t
                   rev
                   (object-name rd)
                   (path->string (opt-source-file opts))
                   (opt-copies opts)
                   size
                   t-cpu
                   t-real
                   t-gc))
    (flush-output)))


#;
(parameterize ([current-command-line-arguments
                (vector "-c" "10" "/Users/sphillips/projects/json_rhm/numbers.json")])
  (run))

(run)