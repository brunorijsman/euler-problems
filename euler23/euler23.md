# Solution for Euler problem 23

In [project Euler problem number 23](http://projecteuler.net/index.php?section=problems&id=23) we are asked to find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

In the problem description we are told that we only need to try the numbers up to 28123.

The function sum_of_divisors(N) to determine the sum of the proper divisors of a number N has been used before, more recently in project Euler problem number 21.

Once we have sum_of_divisors/1, the function is_abundant(N) which determines whether a given number N is an abundant number is trivial to write.

This, in turn, allows us to write all_abundant_numbers_up_to(Max) which computes the list of all abundant numbers up to and including Max in order of increasing value. We use this function to compute all abundant number up to 28123.

The next step is to write function all_pairs_of_abundant_numbers_up_to(Max) which computes a list of which contains all numbers N which satisfy the following two conditions:

1. N = N1 + N2 where N1 and N2 are both amicable numbers.
2. N <= Max.

The next step is to write a function all_non_pairs_up_to(Max) which generates a list of numbers up to and including Max which are not a sum of two amicable numbers. We do this by sorting the list we generated in the previous step and finding all "holes" in the list. Note that we have to deal with the fact that the sorted list contains dupplicates.

Putting it all together we end up with the following Erlang program. Note that we make extensive use of list comprehensions.

The run time of this implementation (on my MacBook Pro) is 41 seconds which is only barely within the allowed one minute. That, along with the rather complex structure of the program, makes me think there is a short-cut which I missed. After punching in my correct solution I browsed the message board for problem 23. It turns out there is a much faster way to find the sum of factors which only loops to sqrt(N) instead of N/2.