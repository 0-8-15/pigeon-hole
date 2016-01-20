# A mailbox constrained by capacity.

* API similar to mailbox.

* No timeouts we love our pigeons too much.

* Flow control.  `send/blocking!` blocks if capacity is exceeded.

# API

    (: isa? (* -> boolean : :dequeue:))

Test predicate for {{PIGEON-HOLE}}.

     (: make (&optional NAME #!key (capacity: 0) -> :dequeue:))

Return a {{PIGEON-HOLE}} contrained by capacity.

    (: size (:dequeue: -> fixnum))

Return number of pigeons in {{PIGEON-HOLE}}.

    (: empty? (:dequeue: -> boolean))

Test {{PIGEON-HOLE}} to be empty.

    (: count (:dequeue: -> fixnum))

Return number of waiters on {{PIGEON-HOLE}}.

    (: send! (:dequeue: VALUE -> boolean))

Immediately send {{VALUE}} to {{PIGEON-HOLE}}.  Does *not* respect
capacity limits!

    (: send/blocking! (:dequeue: * [BLOCK] -> boolean))

Send {{VALUE}} to {{PIGEON-HOLE}}, blocks if capacity is reached.

{{BLOCK}} is either a boolean or a procedure taking the queue as
argument and returning a boolean.  If it is a procedure it is call in
tail position when the call would block.  If {{#f}} does not block but
return #f.  Default if {{#t}}: block for capacity.

Return: {{#t}} if value was send.

    (: receive! (:dequeue: -> *))

Receive value from {{PIGEON-HOLE}}, current thread may block if
capacity is exeeded.

# Examples

    (module
     test
     (test-run)
     (import scheme chicken srfi-18 extras)
     (import (prefix pigeon-hole mailbox-))

    (define mb (mailbox-make 'm0 capacity: 10))

    ;;(define active-mailbox-send! mailbox-send!)
    (define active-mailbox-send! mailbox-send/blocking!)

    (cond-expand
     (compiling
      (define turns 1000000))
     (else
      (define turns 1000)))

    (define tw
      (make-thread
       (lambda ()
	 (do ((i 0 (add1 i)))
	     ((= i turns))
	   (active-mailbox-send! mb i)))
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

      (format #t "~a message passings in ~a (~a per ms)\n " turns (- t1 t0) (/ turns (- t1 t0)))
      )

    ) (import test) (test-run)


# Author

Jörg F. Wittenberger

# License

BSD
