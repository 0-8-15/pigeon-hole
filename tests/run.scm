(declare
 ;;(unit mailbox)
 ;; requirements
 (disable-interrupts)
 ;; promises
 (strict-types)
 (usual-integrations)
 (no-procedure-checks-for-usual-bindings)
 (inline)
 (local)

 (no-bound-checks)
 (no-procedure-checks-for-usual-bindings)
 )


(module
 test
 (test-run)
 (import scheme srfi-18)
 (cond-expand
  (chicken-5
   (import (chicken base))
   (import (chicken format))
   (import (chicken time)))
  (else
   (import chicken extras)
   (use pigeon-hole)))

 (import (prefix pigeon-hole mailbox-))

;; Basic Tests

(let ((mb (mailbox-make 'm0 capacity: 1)))
  (assert (mailbox-empty? mb))
  (mailbox-send! mb 1)
  (assert (not (mailbox-empty? mb)))
  (assert (eq? (mailbox-size mb) 1))
  (assert (eq? (mailbox-send/blocking! mb 1 #f) #f))
  (assert (eq? (mailbox-send/blocking! mb 1 (lambda (x) #f)) #f))
  (assert (eq? (mailbox-receive! mb) 1))
  (assert (eq? (mailbox-size mb) 0))
  (assert (eq? (mailbox-send/blocking! mb 2 #f) #t))
  (assert (eq? (mailbox-size mb) 1))
  (assert (eq? (mailbox-receive! mb) 2))
  (assert (eq? (mailbox-send/anyway! mb 1) #t))
  (assert (eq? (mailbox-send/anyway! mb 2) #t))
  (assert (eq? (mailbox-send/anyway! mb 3) #t))
  (assert (equal? (do ((i 1 (add1 i)) (r '())) ((> i 3) r) (set! r (cons (mailbox-receive! mb) r))) '(3 2 1)))
  )

;; Unstable low level

(let ((mb (mailbox-make 'm0 capacity: 10)))
  (assert (eq? (mailbox-send! mb 1) #t))
  (assert (eq? (mailbox-send! mb 2) #t))
  (assert (eq? (mailbox-send! mb 3) #t))
  (assert (equal? (mailbox-receive-all! mb) '(1 2 3)))
  (assert (eq? (mailbox-size mb) 0))
  (assert (eq? (mailbox-send! mb 1) #t))
  (mailbox-send-list/anyway!! mb '(2 3 4))
  (assert (eq? (mailbox-size mb) 4))
  (assert (equal? (mailbox-receive-all! mb) '(1 2 3 4)))
  )

(define mb (mailbox-make 'm0 capacity: 10))

(cond-expand
 (blocking (define active-mailbox-send! mailbox-send/blocking!))
 (else (define active-mailbox-send! mailbox-send/anyway!)))

(cond-expand
 (compiling
  (define turns 1000000))
 (else
  (define turns 10000)))

(define tw
  (make-thread
   (lambda ()
     (do ((i 1 (add1 i)))
	 ((> i turns))
       (active-mailbox-send! mb i)
       ;; Must be active for my chicken 4.9.1 .
       ;; Otherwise will run into
       ;; "[panic] out of memory - heap full while resizing - execution terminated"
       ;;
       ;;(if (= (modulo i 1000) 999) (gc #t))
       (if (and (eq? active-mailbox-send! mailbox-send!) (= (modulo i 10) 0)) (thread-yield!))
       #;(thread-yield!)
       ))
   'w))

(define tr
  (make-thread
   (lambda ()
     (do ((i 0 (add1 i)))
	 ((= i turns))
       (mailbox-receive! mb)
       ))
   'r))

(define (test-run)
  (thread-start! tr)
  (define t0 (current-milliseconds))
  (thread-start! tw)

  (thread-join! tr)

  (define t1 (current-milliseconds))

  (format #t "~a message~a passed in ~a (~a per ms)\n " turns
	  (if (eq? active-mailbox-send! mailbox-send!) "" "/blocking")
	  (- t1 t0) ( (cond-expand
		       (chicken-5 exact->inexact)
		       (else (lambda (x) x)))
		      (/ turns (- t1 t0))))
  )

 
;;----------

)

(import test)
(test-run)

;; Testing threadpool
(module
 test2
 ()
 (import scheme)
 (cond-expand
  (chicken-5 (import (chicken base)))
  (else (import chicken)))
 
 (import (prefix pigeonry threadpool-))

 (import srfi-18)

 (define poo-type
   (threadpool-make-type
    (lambda (root ex)
      (mutex-specific-set! root ex)
      (mutex-unlock! root))
    (lambda (root)
      (mutex-unlock! root)
      )))

 (define pile (threadpool-make 'pile 1 poo-type))

 (assert
  (equal?
   (let ((mux (make-mutex)))
     (mutex-lock! mux #f #f)
     (threadpool-order! pile mux (lambda (mux) (mutex-specific-set! mux 1)) '())
     (mutex-lock! mux #f #f)
     (mutex-specific mux))
   1))

 )
