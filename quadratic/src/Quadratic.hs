module Quadratic where 

import Data.Complex

data Quadratic = Quadratic { a:: Double, b:: Double, c :: Double} deriving Show

type RootT = Complex Double

data Roots = Roots RootT RootT deriving Show

roots :: Quadratic -> Roots

-- trivial all constants are zero 
roots (Quadratic 0 0 _) = error "Not a quadratic polynomial"

-- polynomial of degree 1; x = -c/b
roots (Quadratic 0.0 b c ) = let root = ((-c) / b :+ 0) in Roots root root

-- b^2 - 4ac = 0

roots (Quadratic a b c) = 
    let discriminant = b*b - 4*a*c 
    in rootsInternal (Quadratic a b c) discriminant

rootsInternal :: Quadratic -> Double -> Roots

-- Discriminant is zero, roots are real
rootsInternal q d | d == 0 = 
    let r = (-(b q) / 2.0 / (a q))
        root = r :+ 0
    in Roots root root

-- Discriminant is negative, roots are complex
rootsInternal q d | d < 0 = Roots (realpart :+ complexpart) (realpart :+ (-complexpart))
    where
        plusd = -d
        twoa = 2.0 * (a q)
        complexpart = (sqrt plusd) / twoa
        realpart = - (b q) / twoa

-- Discriminant is positive, roots are real
rootsInternal q d | d > 0 = Roots (root1 :+ 0) (root2 :+ 0)
    where 
        twoa =  2.0 * (a q)
        dpart = (sqrt d) / twoa
        prefix = - (b q) / twoa
        root1 = prefix + dpart
        root2 = prefix - dpart






