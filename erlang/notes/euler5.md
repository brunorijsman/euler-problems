# Solution for Euler problem 5

In project Euler problem number 5 we are asked to find the smallest number that is evenly divisible by all of the numbers from 1 to 20.

In the first 4 projects I chose the dumb brute force approach. This time I am going to make at least a token effort to introduce some optimizations.

The dumb brute force approach is to try all numbers, starting at 1, until you find a number which is divisible by 1 thru 20.

The first observation is that we don't have to try to divide by all number 1 tru 20. For example, if we try dividing by 20 we don't need to also try dividing by 5 because every number which is divisible by 20 is also divisible by 5. After some consideration, it becomes clear we only have to try dividing by 11 thru 20:

```
1 : don't check, every number is divisible by 1
2 : don't check, already covered by 20
3 : don't check, already covered by 18
4 : don't check, already covered by 20
5 : don't check, already covered by 20
6 : don't check, already covered by 18
7 : don't check, already covered by 14
8 : don't check, already covered by 16
9 : don't check, already covered by 18
10 : don't check, already covered by 20
11 : check
12 : check
13 : check
14 : check
15 : check
16 : check
17 : check
18 : check
19 : check
20 : check
```

The second observation is that we don't every to try each number. For example, we know that the number has to be a multiple of 2, so we only need to try every even number. We know that the number also has to be a multiple of 3, so we only need to try every multiple of 6 (= 2 * 3).

You might be tempted to say: we also know the number has to be a multiple of 4, so we only need to try every multiple of 24 (= 2 * 3 * 4). However this is obviously wrong since 12 is divisible by 2, 3, and 4. The reason for this that 4 is already a multiple of 2.

If we factor each of the numbers 1 thru 20 into their prime factors as shown below we can see that we only need to check multiples of 2 * 2 * 2 * 2 * 3 * 3 * 5 * 7 * 11 * 13 * 17 * 19 = 232792560.

```
1 : 1
2 : 2
3 : 3
4 : 2 2
5 : 5
6 : 2 3
7 : 7
8 : 2 2 2
9 : 3 3
10 : 2 5
11 : 11
12 : 2 2 3
13 : 13
14 : 2 7
15 : 3 5
16 : 2 2 2 2
17 : 17
18 : 2 3 3
19 : 19
20 : 2 2 5
```

We now know that we are looking for the smallest multiple of 232792560 which is … of course … 1 * 232792560 = 232792560.

We don't need to write any problem to solve this -- we already have solved the problem by hand.