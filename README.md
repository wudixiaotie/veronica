veronica
=====

An OTP application

Build
-----

    $ rebar3 compile


Idea
-----
    Use SHA-256 as consistence hash, ring size is 2^256.
```
(veronica@192.168.3.2)5> tc:t(crypto, hash, [sha, <<"asdf1234">>], 10000).
=====================
execute [10000] times of {crypto, hash, [sha,<<"asdf1234">>]}:
Maximum: 52(μs) 5.2e-5(s)
Minimum: 1(μs)  1.0e-6(s)
Sum: 10696(μs)  0.010696(s)
Average: 1.0696(μs)     1.0696e-6(s)
Greater: 422
Less: 9578
=====================
ok
(veronica@192.168.3.2)6> tc:t(crypto, hash, [sha256, <<"asdf1234">>], 10000).
=====================
execute [10000] times of {crypto, hash, [sha256,<<"asdf1234">>]}:
Maximum: 36(μs) 3.6e-5(s)
Minimum: 1(μs)  1.0e-6(s)
Sum: 10891(μs)  0.010891(s)
Average: 1.0891(μs)     1.0890999999999999e-6(s)
Greater: 537
Less: 9463
=====================
ok
```

* 64 nodes on the ring.
