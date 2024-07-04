
class Params{
    type U
    def foo[T](x: T)[U >: x.type <: T](using U)[L <: List[U]](l: L): L = ???
    def aaa(x: U): U = ???
    def bbb[T <: U](x: U)[U]: U = ???
}
