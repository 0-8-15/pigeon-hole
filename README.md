# A mailbox constrained by capacity.

* API similar to mailbox.

* No timeouts we love our pigeons too much.

* Flow control.  `send/blocking!` blocks if capacity is exceeded.

A second module `pigeonry` - currently unstable in API and
undocumented - provides a threadpool.  This is only slightly faster
than creating a fresh thread, catching exceptions and run job it does
while still obeying the capacity limit.
This module **NYD** (not yet documented) at all.

# Documentation

Documented in CHCKEN wiki [pigeon-hole](http://wiki.call-cc.org/eggref/4/pigeon-hole).

# License

BSD
