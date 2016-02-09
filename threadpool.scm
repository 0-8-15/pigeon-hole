(module
 pigeonry
 (pigeonry?
  name
  ;; begin debug only
  threadpool-max threadpool-max-set!
  threadpool-threads threadpool-threads-set!
  threadpool-queue
  threadpool-start!
  ;; end debug only
  make-requesttype
  make-type
  make
  order!
  ;;
  order/anyway!
  order/blocking!
  )
 (import (except scheme max))
 (import chicken (except srfi-18 raise) #;srfi-34 extras)
 (import (prefix pigeon-hole dequeue-))
 
 (define-record threadpool name max threads queue fail success)

 (define pigeonry? threadpool?)
 (define name threadpool-name)
 
 (define-record-printer (threadpool x out)
   (format out "<threadpool ~a (~a) ~a>" (threadpool-name x)
	   (threadpool-max x)
	   (threadpool-queue x)))

 (define-record threadpool-requesttype fail success)
 ;; API *will* change!  Use most specific one to be safe for now.
 (define make-requesttype make-threadpool-requesttype)
 (define make-type make-requesttype)

 (define-record threadpool-request root proc args)

 (cond-expand
  (chicken ;;-never
   (define-inline (%threadpool-queue x) (##sys#slot x 4))
   (define-inline (%threadpool-success x) (##sys#slot x 6))
   (define-inline (%threadpool-request-root x) (##sys#slot x 1))
   (define-inline (%threadpool-request-proc x) (##sys#slot x 2))
   (define-inline (%threadpool-request-args x) (##sys#slot x 3))
   )
  (else
   (define-inline (%threadpool-queue pool) (threadpool-queue pool))
   (define-inline (%threadpool-success pool) (threadpool-success pool))
   (define-inline (%threadpool-request-root x) (threadpool-request-root x))
   (define-inline (%threadpool-request-proc x) (threadpool-request-proc x))
   (define-inline (%threadpool-request-args x) (threadpool-request-args x))
   ))

 (define make-threadpool-queue dequeue-make)
 (define-inline (threadpool-send-message! h v) (dequeue-send! h v))
 (define-inline (threadpool-send-message/blocking! h v) (dequeue-send/blocking! h v))
 (define-inline (threadpool-receive-message! h) (dequeue-receive! h))
 (define-inline (threadpool-queue-empty? h) (dequeue-empty? h))
 (define-inline (threadpool-queue-count h) (dequeue-count h))

 (define-values (make threadpool-start! order/anyway! order/blocking!)
   (let ((%make-threadpool make-threadpool))

     (define (threadpool-start! pool)
       (let ((mx (threadpool-max pool)))
	 (if (cond
	      ((number? mx) (if (fx<= mx 0) #f (begin (threadpool-max-set! pool (fx- mx 1)) #t)))
	      ((boolean? mx) mx)
	      (else (error "unknown threadpool limit" mx)))
	     (let ((t (make-thread pool-thread-loop (threadpool-name pool))))
	       (thread-specific-set! t pool)
	       (thread-start! t))))
       pool)

     (define (pool-thread-loop)
       (define pool (thread-specific (current-thread)))
       (define success (%threadpool-success pool))
       (if (number? (threadpool-max pool))
	   (threadpool-max-set! pool (sub1 (threadpool-max pool))))
       (threadpool-threads-set! pool (cons (current-thread) (threadpool-threads pool)))
       (do ((entry #f) (exn #f))
	   (#f)
	 ( ;; guard (ex (else (set! exn (list ex))))
	  handle-exceptions ex (set! exn (list ex))
	  (do ()
	      (#f)
	    (if exn
		(if entry
		    (let ((e entry) (r (car exn)))
		      (set! entry #f)
		      (set! exn #f)
		      ((threadpool-fail pool) (%threadpool-request-root e) r))
		    (begin
		      (print-error-message exn (current-error-port) "Thread Pool")
		      (format (current-error-port) "thread pool ~s\n" (threadpool-name pool))
		      (print-call-chain (current-error-port) 0 (current-thread)))))
	    (let ((queue (%threadpool-queue pool)))
	      (set! entry (threadpool-receive-message! queue))
	      (or (> (threadpool-queue-count queue) 0)
		  ;;(threadpool-queue-empty? queue)
		  (threadpool-start! pool)))
	    (let ((args (%threadpool-request-args entry))
		  (root (%threadpool-request-root entry)))
	      (if (null? args)
		  ((%threadpool-request-proc entry) root)
		  (let ((a1 (##sys#slot #;car args 0)) (r (##sys#slot #;cdr args 1)))
		    (if (null? r)
			((%threadpool-request-proc entry) root a1)
			(apply (%threadpool-request-proc entry) (cons root args)))))
	      (and success (success root)))))))

     (define (make-threadpool name max type)
       (let ((pool (%make-threadpool
		    name max '() (make-threadpool-queue name capacity: 0)
		    (threadpool-requesttype-fail type) (threadpool-requesttype-success type))))
	 #;(if (number? max) (do ((i 1 (add1 i))) ((= i max)) (threadpool-start! pool)))
	 (threadpool-start! pool)))

     (define (threadpool-order! pool root proc args)
       (threadpool-send-message! (threadpool-queue pool) (make-threadpool-request root proc args))
       #;pool)
     (define (threadpool-order/blocking! pool root proc args)
       (threadpool-send-message/blocking! (threadpool-queue pool) (make-threadpool-request root proc args))
       #;pool)
     (values make-threadpool threadpool-start! threadpool-order! threadpool-order/blocking!)))

 (define order! order/blocking!)

 )
