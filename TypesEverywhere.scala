class TypesEverywhere{
    def f1[T](x: T): T = x
    def f2[T][U](x: T, y: U): (T, U) = (x, y)
    def f3[T](x: T)[U](y: U): (T, U) = (x, y)
}