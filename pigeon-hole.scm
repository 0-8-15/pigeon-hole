(declare
 (disable-interrupts) ;; a must

 (no-bound-checks)
 (no-procedure-checks)
 (local)
 (inline)
 (safe-globals)
 (specialize)
 ;;(strict-types)

 )

(module
 pigeon-hole
 (make isa? empty? await-message! send/anyway! send/blocking! send! receive! count size name
       ;; low level, unstable API
       send-list/anyway!! receive-all!)
 (import scheme)
 (cond-expand
  (chicken-5
   (import (only (chicken format) format))
   (import (chicken base))
   (import (chicken type))
   (import (chicken fixnum))
   (import srfi-18))
  (else
   (import chicken)
   (use srfi-18 (only extras format))))

 (define-record <dequeue> waiting block count qh qt)

 (define-type :dequeue: (struct <dequeue>))

 (: isa? (* -> boolean : :dequeue:))
 (define isa? <dequeue>?)

 (cond-expand
  (never
   (define-inline (dequeue-waiting mb) (<dequeue>-waiting mb))
   (define-inline (dequeue-block mb) (<dequeue>-block mb))
   (define-inline (dequeue-count mb) (<dequeue>-count mb))
   (define-inline (dequeue-count-set! mb v) (<dequeue>-count-set! mb v))
   (define-inline (dequeue-qh mb) (<dequeue>-qh mb))
   (define-inline (dequeue-qh-set! mb v) (<dequeue>-qh-set! mb v))
   (define-inline (dequeue-qt mb) (<dequeue>-qt mb))
   (define-inline (dequeue-qt-set! mb v) (<dequeue>-qt-set! mb v)))
  (else
   (define-inline (dequeue-waiting mb) (##sys#slot mb 1))
   (define-inline (dequeue-block mb) (##sys#slot mb 2))
   (define-inline (dequeue-count mb) (##sys#slot mb 3))
   (define-inline (dequeue-count-set! mb v) (##sys#setislot mb 3 v))
   (define-inline (dequeue-qh mb) (##sys#slot mb 4))
   (define-inline (dequeue-qh-set! mb v) (##sys#setslot mb 4 v))
   (define-inline (dequeue-qt mb) (##sys#slot mb 5))
   (define-inline (dequeue-qt-set! mb v) (##sys#setslot mb 5 v))
   ))

 (define-record-printer (<dequeue> x out)
   (format out "<queue ~a (size ~a capacity ~a~a)>" (name x) (size x) (count x)
	   (if (thread? (mutex-state (dequeue-block x))) " blocking" "")))

 (define-inline (%make-empty-queue) (cons 0 '()))

 ;; FIXME how to declare this? (: make (&optional * &rest fixnum -> :dequeue:))
 (define make
   (let ((make-dequeue make-<dequeue>))
     (lambda (#!optional (name #f) #!key (capacity 0))
       (let ((x (%make-empty-queue)))
	 (make-dequeue
	  (make-condition-variable name) (make-mutex name)
	  capacity x x)))))

 (: empty? (:dequeue: -> boolean))
 (define (empty? queue) (null? (cdr (dequeue-qh queue))))
 (: count (:dequeue: -> fixnum))
 (define count dequeue-count)
 (define (name queue) (condition-variable-name (dequeue-waiting queue)))
 (: size (:dequeue: -> fixnum))
 (define-inline (%size queue) (car (dequeue-qh queue)))
 (define (size queue) (%size queue))

 (define unlocked (make-mutex 'unlocked))
 
 (: await-message! (:dequeue: -> undefined)) ;; sort-of deprecated
 (define (await-message! queue)
   (dequeue-count-set! queue (add1 (dequeue-count queue)))
   ;; this is only safe if mutex-unlock! does not switch threads
   ;; until waiting on the condition variable.
   (mutex-unlock! (dequeue-block queue))
   (dynamic-wind
       void
       (lambda () (mutex-unlock! unlocked (dequeue-waiting queue)))
       (lambda () (dequeue-count-set! queue (sub1 (dequeue-count queue))))))

 (: send/anyway! (:dequeue: * -> boolean))
 (define (send/anyway! queue job)
   (let ((job (cons job '()))
	 (t (dequeue-qt queue)))
     (set-cdr! t job)
     (dequeue-qt-set! queue job)
     (let ((p (dequeue-qh queue)))
       (##sys#setislot p 0 (fx+ (##sys#slot p 0) 1))
       #;(set-car! p (add1 (car p)))
       ))
   (condition-variable-signal! (dequeue-waiting queue))
   #t)

 (: send/blocking! (:dequeue: * &rest (or boolean (procedure (:dequeue:) boolean)) -> boolean))
 (define (send/blocking! queue job #!optional (block #t))
   #;(assert (or (boolean? block) (procedure? block)))
   (let loop ()
     (if (fx> (count queue) (%size queue))
	 (begin
	   (send/anyway! queue job)
	   (let ((mux (dequeue-block queue)))
	     ;; The commented-out case is only required to cope with a CHICKEN bug.
	     (if (eq? (mutex-state mux) 'not-abandoned) #;(or (eq? (mutex-state mux) 'not-abandoned) (eq? (mutex-state mux) 'abandoned))
		 (mutex-unlock! mux)))
	   #t)
	 (cond
	  ;; Note: Using `mutex-lock!` without #f as thread incures
	  ;; about 20% overhead.
	  ((eq? block #t) (mutex-lock! (dequeue-block queue) #f #f) (loop))
	  ((not block) #f)
	  ((procedure? block) (block queue))))))

 (: send! (:dequeue: * -> boolean))
 (define (send! queue job) (send/blocking! queue job))
 
 (: receive! (:dequeue: -> *))
 (define (receive! queue)
   (let loop ()
     (if (empty? queue)
	 (begin
	   (await-message! queue)
	   (loop))
	 (let* ((p0 (dequeue-qh queue))
		(p (##sys#slot p0 1) #;(cdr p0))
		(len (##sys#slot p0 0) #;(car p0)))
	   (dequeue-qh-set! queue p)
	   (let ((x (##sys#slot p 0) #;(car p)))
	     (##sys#setislot p 0 (fx- len 1)) #;(set-car! p (fx- len 1))
	     x)))))

 
 ;; Low level / unstable API

 (: send-list/anyway!! (:dequeue: (list-of *) &rest fixnum pair -> undefined))

 (define (send-list/anyway!! queue msgs #!optional (len #f) (last #f))
   (if (pair? msgs) ;; NOOP if null?
       (begin
	 (if (not len)
	     (do ((msgs msgs (cdr msgs)) (n 1 (fx+ n 1)))
		 ((null? (cdr msgs)) (set! len n) (set! last msgs))))
	 (let ((h (dequeue-qh queue)))
	   (set-car! h (fx+ (car h) len))
	   (set-cdr! (dequeue-qt queue) msgs)
	   (dequeue-qt-set! queue last)
	   (condition-variable-signal! (dequeue-waiting queue))))))
 
 (: receive-all! (:dequeue: -> (list-of *)))

 (define (receive-all! queue)
   (if (empty? queue) '()
       (let ((msgs (cdr (dequeue-qh queue)))
	     (nq (%make-empty-queue)))
	 (dequeue-qh-set! queue nq)
	 (dequeue-qt-set! queue nq)
	 (mutex-unlock! (dequeue-block queue))
	 msgs)))
 
 )
