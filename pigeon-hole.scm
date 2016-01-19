(use srfi-18 extras)

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
 (make isa? empty? await-message! send! send/blocking! receive! count size name)
 (import scheme chicken srfi-18)
 (import (only extras format))

 (define-record <dequeue> waiting block count queue)

 (define-type :dequeue: (struct <dequeue>))

 (: isa? (* -> boolean : :dequeue:))
 (define isa? <dequeue>?)

 (cond-expand
  (never
   (define-inline (dequeue-waiting mb) (<dequeue>-waiting mb))
   (define-inline (dequeue-block mb) (<dequeue>-block mb))
   (define-inline (dequeue-count mb) (<dequeue>-count mb))
   (define-inline (dequeue-count-set! mb v) (<dequeue>-count-set! mb v))
   (define-inline (dequeue-queue mb) (<dequeue>-queue mb)))
  (else
   (define-inline (dequeue-waiting mb) (##sys#slot mb 1))
   (define-inline (dequeue-block mb) (##sys#slot mb 2))
   (define-inline (dequeue-count mb) (##sys#slot mb 3))
   (define-inline (dequeue-count-set! mb v) (##sys#setislot mb 3 v))
   (define-inline (dequeue-queue mb) (##sys#slot mb 4))
   ))

 (define-record-printer (<dequeue> x out)
   (format out "<queue ~a (size ~a capacity ~a~a)>" (name x) (size x) (count x)
	   (if (thread? (mutex-state (dequeue-block x))) " blocking" "")))

 (: make (&optional * &rest -> :dequeue:))
 (define make
   (let ((make-dequeue make-<dequeue>))
     (lambda (#!optional (name #f) #!key (capacity 0))
       (make-dequeue
	(make-condition-variable name) (make-mutex name)
	capacity
	(let ((x (list 0))) (cons x x))))))

 (: empty? (:dequeue: -> boolean))
 (define (empty? queue) (null? (cdar (dequeue-queue queue))))
 (: count (:dequeue: -> fixnum))
 (define count dequeue-count)
 (define (name queue) (condition-variable-name (dequeue-waiting queue)))
 (: size (:dequeue: -> fixnum))
 (define (size queue) (caar (dequeue-queue queue)))

 (define unlocked (make-mutex 'unlocked))
 
 (: await-message! (:dequeue: -> undefined)) ;; sort-of deprecated
 (define (await-message! queue)
   (dequeue-count-set! queue (add1 (dequeue-count queue)))
   ;; this is only safe if mutex-unlock! does not switch threads
   ;; until waiting on the condition variable.
   (mutex-unlock! (dequeue-block queue))
   (mutex-unlock! unlocked (dequeue-waiting queue))
   (dequeue-count-set! queue (sub1 (dequeue-count queue))))

 (: send! (:dequeue: * -> boolean))
 (define (send! queue job)
   (let ((job (cons job '())))
     (set-cdr! (cdr (dequeue-queue queue)) job)
     (set-cdr! (dequeue-queue queue) job))
   (condition-variable-signal! (dequeue-waiting queue))
   #t)

;; `send/blocking!` needs roughly 3x as much runtime as `send!` in
;; contention case and incures roughly 20% overhead otherwise

 (: send/blocking! (:dequeue: * -> boolean))
 (define (send/blocking! queue job)
   (let loop ()
     (if (> (count queue) 0)
	 (begin (send! queue job) (mutex-unlock! (dequeue-block queue)))
	 (begin (mutex-lock! (dequeue-block queue)) (loop))))
   #t)

 (: receive! (:dequeue: -> *))
 (define (receive! queue)
   (let loop ()
     (if (empty? queue)
	 (begin
	   (await-message! queue)
	   (loop))
	 (let* ((queue (dequeue-queue queue))
		(len (caar queue)))
	   (set-car! queue (cdar queue))
	   (let ((x (caar queue))) (set-car! (car queue) (sub1 len)) x)))))

 )
