-module(euler34).

-export([solve/0]).

fac(0) -> 1;
fac(1) -> 1;
fac(2) -> 2;
fac(3) -> 6; 
fac(4) -> 24; 
fac(5) -> 120; 
fac(6) -> 720; 
fac(7) -> 5040; 
fac(8) -> 40320; 
fac(9) -> 362880. 

sum_digit_facs(0) -> 0;
sum_digit_facs(N) -> fac(N rem 10) + sum_digit_facs(N div 10).

check(N, Sum, Bound) when N >= Bound -> Sum;

check(N, Sum, Bound) ->
    case sum_digit_facs(N) of
        N -> check(N + 1, Sum + N, Bound);
        _ -> check(N + 1, Sum, Bound)
    end.

solve() ->
    Bound = fac(9) * 7, 
    check(10, 0, Bound).
    
