:- module(charmichael, [charmichael/2]).
/*
    An integer n is a Carmichael number iff:
        n is square-free
        for every prime divisor p of n:
            p - 1 | n - 1
*/
head([X | Y], X).
tail([X | Y], Y).
tail([X | []], []). % this is technically correct anyway

% divisibility function
divides(P, Q) :-
    P mod Q =:= 0.

% finds a factorization of N (except 1 and N)
factorization(N, L) :-
    factorization_prime(N, 2, L), !.
factorization_prime(N, M, L) :-
    N_sqrt is sqrt(N),
    N_sqrt < M,

    L = []. % N is prime
factorization_prime(N, M, L) :-
    N_sqrt is sqrt(N),
    N_sqrt >= M,

    R is N rem M,
    R == 0,
    D is N div M,

    L = [M, D].
factorization_prime(N, M, L) :-
    M_prime is M + 1,
    factorization_prime(N, M_prime, L).

% computes the prime factorization of N
primeFactorization(N, Q) :-
    primeFactorization_prime(N, [N], [], Q).

primeFactorization_prime(_N, [], P, Q) :-
    Q = P.
primeFactorization_prime(N, [FH |FT], P, Q) :-
    factorization(FH, F_factors),
    pfp_helper([FH|FT], F_factors, N, P, P_prime, F_prime),
    primeFactorization_prime(N, F_prime, P_prime, Q).

% prime factor case
pfp_helper([F_head | F_tail], [], N, P, P_prime, _) :-
    F_head   \= N,     % or else we end up putting N in P if N is prime
    append([F_head], P, P_prime),
    F_prime = F_tail.
pfp_helper([_ | F_tail], F_factors, _N, P, P_prime, F_prime) :-
    F_factors = [_ | _],
    append(F_factors, F_tail, F_prime),
    P_prime = P.



% check that p - 1 | n - 1
checkPrimeFactors(N) :-
    primeFactorization(N, P),
    checkPrimeFactors_prime(N, P).
checkPrimeFactors_prime(N, []).
checkPrimeFactors_prime(N, P) :-
    head(P, P_head),
    tail(P, P_tail),

    N_prime is N - 1,
    P_prime is P_head - 1,

    N_prime > 0,
    P_prime > 0,

    divides(N_prime, P_prime),

    checkPrimeFactors_prime(N, P_tail).

% determines whether a number is square-free
squareFree(N) :-
    squareFree_prime(N, 2).
squareFree_prime(N, M) :-   % our square is bigger than N - no square factor
    M_square is M * M,
    M_square > N.
squareFree_prime(N, M) :-   % our square doesn't divide N - no square factor
    M_square is M * M,
    M_square =< N,
    M_prime is M + 1,

    not(divides(N, M_square)),
    squareFree_prime(N, M_prime).

% generates all carmichael numbers up to N, inclusive
carmichael(N, L) :-
    carmichael_prime(N, 1, [], Q),
    L = Q.
carmichael_prime(N, M, P, Q) :-
    N < M,
    Q = P.
carmichael_prime(N, M, P, Q) :-
    M_prime is M + 1,
    checkCarmichael(M),
    append([M], P, P_prime),

    carmichael_prime(N, M_prime, P_prime, Q).
carmichael_prime(N, M, P, Q) :-
    M_prime is M + 1,
    carmichael_prime(N, M_prime, P, Q).

checkCarmichael(N) :-
    squareFree(N),
    checkPrimeFactors(N).
