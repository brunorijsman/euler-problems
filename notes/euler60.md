# Solution for Euler problem 60

First we prepare a PairTable which maps every prime P<sub>1</sub> (up to MaxPrime) to a list pair primes P<sub>2</sub> (where P<sub>2</sub> > P<sub>1</sub>) such that concat(P<sub>1</sub>,P<sub>2</sub>) and concat (P<sub>2</sub>,P<sub>1</sub>) are primes as well.

MaxPrime is emperically guessed larger than any number in the solution set. We picked 9999 which turned out to be large enough to find the solution and not too large to exceed the 1 minute run time limit.

Once the PairTable is prepared, we search for the solution set [P1, P2, P3, P4, P5] by recursively extending partial solutions.

If we have a partial solution [P<sub>1</sub>, ..., P<sub>N</sub>] we try adding P<sub>N+1</sub> and verify that P<sub>N+1</sub> is a pair (in the PairTable) for P<sub>1</sub> thru P<sub>N</sub>.

The order in which the solutions are explored is as follows:

* P<sub>1</sub> visits every prime 2 ... MaxPrime.

* P<sub>2</sub> visits every prime P<sub>1</sub>+1 ... MaxPrime.

* Etc.

Once we find the first set [P1, P2, P3, P4, P5] which meets the criteria we also know it is the set with the lowest sum because of the order in which the sets are explored.
