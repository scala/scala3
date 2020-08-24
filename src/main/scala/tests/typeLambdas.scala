package tests

package typeLambdas

type L1 = Int
type U1 = String
type R1 = Double

type TL1  =  [X, Y] =>> Map[X,Y]

type TL2 = [X >: Int] =>> [Y <: String] =>> Map[X, Y]