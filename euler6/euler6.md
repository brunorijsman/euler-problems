# Solution for Euler problem 6

Euler problem 6 asks us to compute the difference between the sum of squares and the square of the sums for the numbers 1 ... 100.

I remembered the formula for adding up the integers from 1 to N from an amusing story about how Gauss solved it when he was 10 years old:

1 + 2 + 3 + ... + n = n(n+1)/2.

I did not remember a formula for the sum of squares, so I computed that brute force.

After I wrote the Erlang program I found that there is in fact a formula (the square pyramidal number which is a special case of Faulhaber's formula) for the sum of squares too:

1^2 + 2^2 + 3^2 + ... + n^2 = (2n^3 + 3n^2 + n)/2
