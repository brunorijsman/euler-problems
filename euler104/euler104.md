# Solution for Euler problem 104

My first approach was to simply iterate over the sequence of Fibonacci numbers, and for each number to check whether last and the first 9 digits are pandigital.

Since it is easier to find the last 9 digits of a number than to find the first 9 digits, I check the last 9 digits first.

This approach did indeed find the solution, but it took approximately 3 minutes.

I tried speeding up the solution by only checking for pandigitals if the first 9 digits of the Fibonaci number changed. This did not have the desired effect - it slowed things down a lot. The reason was that this required extracting the first 9 digits for every first Fibonacci number instead of only those numbers whose last 9 digits had already been determined to be pandigital.

I then used the Erlang profiles (fprof) to find the bottlenecks in the implementation.

I made the following optimizations.

I changed nr_to_list avoid appending to the tail of the list:

```
%% Original code

nr_to_list(N) when N < 10 ->
    [N];

nr_to_list(N) ->
    nr_to_list(N div 10) ++ [N rem 10].
```

```
%% New code

nr_to_list_rev(N) when N < 10 ->
    [N];

nr_to_list_rev(N) ->
    [N rem 10 | nr_to_list_rev(N div 10)].

nr_to_list(N) ->
    lists:reverse(nr_to_list_rev(N)).
```

I made first_9_digits more efficient by chopping of bigger chunks of trailing digits in each iteration as long as the number is big enough to allow it:

```
%% Original code

first_9_digits(N) when N > 999999999 ->
    first_9_digits(N div 10);

first_9_digits(N) -> 
    N.
```

```
%% New code

first_9_digits(N) when N >= 100000000000000000 ->
    first_9_digits(N div 1000000000);

first_9_digits(N) when N >= 1000000000 ->
    first_9_digits(N div 10);

first_9_digits(N) -> 
    N.
```

At this point the code was fast enough - it found the solution in about 40 seconds.

I explored different implementations of is_pandigital:

```
%% Original code

contains_digit(NrList, Digit, Contains) ->
    Contains andalso lists:any(fun(Element) -> Element == Digit end, NrList).

range_fold(_Fun, Acc0, Start, End) when Start > End ->
    Acc0;

range_fold(Fun, Acc0, Start, End) ->
    range_fold(Fun, Fun(Start, Acc0), Start + 1, End).
    
is_pandigital(N) ->
    NList = nr_to_list(N),
    range_fold(fun(Digit, Contains) -> contains_digit(NList, Digit, Contains) end, true, 1, 9).
```

```
%% Alternative 1

is_pandigital(N) ->
    NList = nr_to_list(N),
    lists:sort(NList) == [1, 2, 3, 4, 5, 6, 7, 8, 9].
```

```
%% Alternative 2

%% Assumes caller has already made sure that N is exactly 9 digits long
is_pandigital(N) ->
    is_pandigital(N, false, false, false, false, false, false, false, false, false).

is_pandigital(0, S1, S2, S3, S4, S5, S6, S7, S8, S9) ->
    S1 and S2 and S3 and S4 and S5 and S6 and S6 and S7 and S8 and S9;

is_pandigital(N, S1, S2, S3, S4, S5, S6, S7, S8, S9) ->
    case N rem 10 of
        0 -> false;
        1 -> is_pandigital(N div 10, true, S2, S3, S4, S5, S6, S7, S8, S9);
        2 -> is_pandigital(N div 10, S1, true, S3, S4, S5, S6, S7, S8, S9);
        3 -> is_pandigital(N div 10, S1, S2, true, S4, S5, S6, S7, S8, S9);
        4 -> is_pandigital(N div 10, S1, S2, S3, true, S5, S6, S7, S8, S9);
        5 -> is_pandigital(N div 10, S1, S2, S3, S4, true, S6, S7, S8, S9);
        6 -> is_pandigital(N div 10, S1, S2, S3, S4, S5, true, S7, S8, S9);
        7 -> is_pandigital(N div 10, S1, S2, S3, S4, S5, S6, true, S8, S9);
        8 -> is_pandigital(N div 10, S1, S2, S3, S4, S5, S6, S7, true, S9);
        9 -> is_pandigital(N div 10, S1, S2, S3, S4, S5, S6, S7, S8, true)
    end.
```

It turned out that all 3 implementation were about the same speed.  Since alternative 1 is by far the easiest to read, I used alternative 1.

At this point I called it a day -- finding a solution in 40 seconds is 'good enough'.
