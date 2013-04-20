# Solution for Euler problem 63

We are looking for all numbers N which have the following two properties:

* N = b<sup>p</sup> where b is the base and p is the power

* N has p digits

We know that b < 10. 

If b >= 10 then b<sup>p</sup> will have at least p+1 digits which is too many.

We also know that if a<sup>p</sup> has fewer than p digits, then a<sup>q</sup> for all q>p also has fewer than p digits.

This is easily seen using induction: a<sup>p+11</sup> = a a<sup>p</sup> < 10 a<sup>p</sup>

This leads to a simple procedure:

* Try all bases b from 1 to 10

* For each base b try all powers p starting at 1 until we find a number which has too few digits.

