package tests

package typeLambdas

type L1 = Int
type U1 = String
type R1 = Double

type Id[T <: AnyKind]

type TL1  =  Id[[X, Y] =>> Map[X,Y]]

type TL2 = Id[[X >: Int] =>> [Y <: String] =>> Map[X, Y]]

type LabdaA = List[(x: String) => List[x.type]]
