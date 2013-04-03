# solution for Euler problem 62

### The wrong approach

This problem stumped for for a long time because I originally went down a dead end approach.

My original approach was:

- Visit each cube number C from 1 to infinity

- Generate all permutations P of C

- Count how many value of P are cube

- If the count is 5 we have found the solution.

This solution was far too slow.

### The correct approach

- Keep a map of NormalizedCube -> Count

- Visit every cube number C from one to infinity

- Normalize the cube C as described below

- Increase the count for the NormalizedCube in the map

- If the Count in the map reaches 5 we have found our solution

The intent of normalizing a number is to make sure that two numbers N1 and N2 map to the same NormalizedN if (and only if) N1 and N2 are permutations of each other.

Normalizing the number N is done as follows:

- Sorting the digits in the number.

- Counting the number of zeroes in the number (otherwise the normalized values for 2001 and 201 are the same, namely 12).