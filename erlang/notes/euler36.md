# Solution for Euler problem 32

It would have been very straightforward to simply iterate over all numbers from 1 to 999,999 and check whether the number is a palindrome in both decimal and binary representation.  It would probably have finished easily within the limit of one minute.

I decided to make it a little bit more interesting a implement a much more efficient algorithm.

It is very easy to generate all palindromes of base-10 numbers of N digits.

If N is even (say N=8), you simply generate all numbers of N/2 digits. For every generated number (say abcd) you can produce a palindrome of length N by appending the mirrored number (say abcd ++ dcba = abcddcba).

If N is odd (say N=7), you simply generate all numbers of (N+1)/2 digits. For every generated number (say abcd) you produce a palindrome of length N by dropping the last digit (d) and appending the mirrored remaining number (say abcd ++ cdba = abcdcba).

Once all palindromes of base-10 numbers of N digits are produced, you only need to convert each to binary and checked whether it is also a palindrome in binary.