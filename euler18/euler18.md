# Solution for Euler problem 18

In [project Euler problem number 18](http://projecteuler.net/index.php?section=problems&id=18) we are asked to determine the maximum sum traveling from the top of a triangle to the base.

This time, I did not solve the problem using the brute-force approach of trying all possible roots from the top to the bottom.

Instead, I rely on the observation that the cost from a to the bottom = cost from a to the bottom + max(cost from b to the bottom, cost from c to the bottom):

```
     x
    x x
   x x x
  a x x x
 b c x x x
x x x x x x
```

This is used to "collapse" the bottom two rows of the triangle into one as follows:

```
     x                         x
    x x                       x x
   x x x         ==>         x x x
  x x x x                   x x x x
 a x x x x                 d x x x x
b c x x x x
```

where d = a + max(b,c).

This process is repeated until there is one remaining row which contains the answer.
