# Solution for Euler problem 7

Once again, my Erlang solution uses the brute force approach with a few optimizations.

I check all odd numbers for being a prime number until I find the 10001st prime.

To check whether number n is prime, I try to divide it by all numbers from 2 thru sqrt(n).

At the time I wrote the Erlang implementation I was aware of the [sieve of Eratosthenes](http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) as a method to find all prime numbers up to some number n (which is not exactly the same question as Euler problem 7) but I did not know how to implement it in Erlang. 

It turns out that there there is a [stack overflow question](http://stackoverflow.com/questions/146622/sieve-of-eratosthenes-in-erlang) which discusses this and contains several example implementations. There is also [this excellent article](http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf) on implementing the sieve in functional languages.