# Solution for Euler problem 27

We are considering formulas of the following form:

f(n) = n^2 + an + b where |a| < 1000 and |b| < 1000

such that

f(0), f(1), f(2), …., f(N) are all prime for some value of N

f(0) is prime means that b is prime.

We know |b| < 1000 but we also know that there are no prime numbers less than 1, so we can say 0 < b < 1000.

f(1) is prime means that a + b is prime.

We know that:

```
|a| < 1000                     =>
-1000 < a < 1000               =>
-1000 + b < a + b < 1000 +b    => (a + b is prime AND all prime numbers are >= 1 AND b < 1000)
0 < a + b < 1000 + b           =>
-b < a < 1000
```

This leads to the following algorithm:

```
Let b iterate over all prime numbers between 1 and 1000.
|
| Let a iterate over -b+1 to 999
| |
| | Find the maximum N such that n^2 + an + b is prime for n where 1 <= n <= N 
| |
| | Remember the highest value N for all considered values of a and b
```

We need two things for this:

1. A fast way to check wether a given number k is prime. I implement this by simply trial divisions by 1 … sqrt(k).

2. Note that we will likely check the same number k many times. I thought it wouldl be useful to maintain a cache of numbers already checked previously, but the solution was plenty fast without it.

3. A fast way to iterate over all primes from 1 to 1000. Since the range is small, we will simply iterate over all numbers 1…1000 and check each number for primality.
