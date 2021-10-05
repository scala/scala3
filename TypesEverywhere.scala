import scala.annotation.targetName
class TypesEverywhere{
    def f1[T](x: T): T = x
    def f2[T][U](x: T, y: U): (T, U) = (x, y)
    def f3[T](x: T)[U <: x.type](y: U): (T, U) = (x, y)

    
    
    def f4[T](x: T)(y: T) = (x,y)
    @targetName("f5") def f4[T](x: T, y: T) = (x,y)
}


