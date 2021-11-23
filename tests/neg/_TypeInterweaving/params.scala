class Params{
    def bar[T](x: T)[T]: String = ??? // error
    def zoo(x: Int)[T, U](x: U): T = ??? // error
    def bbb[T <: U](x: U)[U]: U = ??? // error // error
}