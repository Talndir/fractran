import Data.Maybe

type Factor = (Int, Int)
type Number = [Factor]
type Fraction = (Number, Number)
type Program = [Fraction]

-- Multiplication - given 2^x.3^y returns 5^xy
-- Program: 13/3.7, 5.7.11/13, 1/7, 11/3, 7/2, 1/3
multProg = [([(13, 1)], [(3, 1), (7, 1)]),
            ([(5, 1), (7, 1), (11, 1)], [(13, 1)]),
            ([],[(7, 1)]),
            ([(3, 1)], [(11, 1)]),
            ([(7, 1)], [(2, 1)]),
            ([], [(3, 1)])] :: Program

-- Hamming distance - given 2^a returns 13^(number of 1s in binary rep of a)
-- Program: 3.11/2^2.5, 5/11, 13/2.5, 1/5, 2/3, 2.5/7, 7/2
hammProg = [([(3, 1), (11, 1)], [(2, 2), (5, 1)]),
            ([(5, 1)], [(11, 1)]),
            ([(13, 1)], [(2, 1), (5, 1)]),
            ([], [(5, 1)]),
            ([(2, 1)], [(3, 1)]),
            ([(2, 1), (5, 1)], [(7, 1)]),
            ([(7, 1)], [(2, 1)])] :: Program

join :: Number -> Maybe Number -> Maybe Number
join _ Nothing    = Nothing
join xs (Just ys) = Just (join' xs ys)
    where
    join' :: Number -> Number -> Number
    join' xs [] = xs
    join' [] ys = ys
    join' n1@((a, p):xs) n2@((b, q):ys)
        | a < b     = (a, p) : (join' xs n2)
        | a > b     = (b, q) : (join' n1 ys)
        | otherwise = (a, p + q) : (join' xs ys)

multiply :: Number -> Fraction -> Maybe Number
multiply as (xs, []) = join as (Just xs)
multiply [] _        = Nothing
multiply ((a, p):as) frac@(xs, (b, q):ys)
    | a < b     = join [(a, p)] (multiply as frac)
    | a > b     = Nothing
    | p < q     = Nothing
    | p == q    = multiply as (xs, ys)
    | otherwise = join [(a, p - q)] (multiply as (xs, ys))

run :: Number -> Program -> Number
run n p = run' n p p
    where
    run' :: Number -> Program -> Program -> Number
    run' n [] _ = n
    run' n (f:fs) p
        | r == Nothing = run' n fs p
        | otherwise    = run' (fromJust r) p p
            where
            r = multiply n f
