# Solution for Euler problem 9

In Euler problem 9 we are asked to find the one and only Pythagoran triplet for which a+b+c=1000.

We simply try all combinations of three numbers and check whether each is the solution we are looking for.

The main the complication is the order in which all combinations of three numbers are generated. The following example illustrates this:

```
1 2 3
1 2 4
1 3 4
2 3 4
1 2 5
1 3 5
2 3 5
1 4 5
2 4 5
3 4 5
1 2 6
1 3 6
2 3 6
1 4 6
2 4 6
3 4 6
1 5 6
2 5 6
...
```