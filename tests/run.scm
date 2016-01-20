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

(use pigeon-hole)


(module
 test
 (test-run)
 (import scheme chicken srfi-18 extras)
 (import (prefix pigeon-hole mailbox-))

;; Basic Tests

(let ((mb (mailbox-make 'm0 capacity: 1)))
  (mailbox-send! mb 1)
  (assert (eq? (mailbox-size mb) 1))
  (assert (eq? (mailbox-send/blocking! mb 1 #f) #f))
  (assert (eq? (mailbox-send/blocking! mb 1 (lambda (x) #f)) #f))
  (assert (eq? (mailbox-receive! mb) 1))
  (assert (eq? (mailbox-size mb) 0))
  (assert (eq? (mailbox-send/blocking! mb 2 #f) #t))
  (assert (eq? (mailbox-size mb) 1))
  (assert (eq? (mailbox-receive! mb) 2))
  )
 
(define mb (mailbox-make 'm0 capacity: 10))

(cond-expand
 (blocking (define active-mailbox-send! mailbox-send/blocking!))
 (else (define active-mailbox-send! mailbox-send!)))

(cond-expand
 (compiling
  (define turns 1000000))
 (else
  (define turns 1000)))

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
	  (- t1 t0) (/ turns (- t1 t0)))
  )

 
;;----------

)

;; Still the problem with chicken being slow without useless uses.
(use srfi-1)

(import test)
(test-run)
