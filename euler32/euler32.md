# Solution for Euler problem 32

## Problem

[Project Euler problem 32](http://projecteuler.net/problem=32) is as follows:

We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 * 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

## Approach

We start with a more specific problem: find all products whose multiplicand / multiplier / product can be written as N pandigital (where 1 <= N <= 9).

For a given value of N we generate all permutations of the digits 1 … N.

Consider, as an example, that N is 9 and we are considering one specific permutation of the digits 1…9 namely
 391867254.
 
For that single permutation, we consider all the possible positions to split up the permutation into product of two numbers:

```
3 * 9 = 1867254             no
3 * 91 = 867254             no
3 * 918 = 67254             no
…
39 * 1 = 867254             no
39 * 18 = 67254             no
39 * 186 = 7254             YES
39 * 1867 = 254             no
…
391 * 8 = 67254             no
391 * 86 = 7254             no
...
```

Once we find a product which meets the requirements, we add it to a dictionary (to weed out the dupplicates as mentioned in the hint).

At the end, we add up all the products in the dictionary.
