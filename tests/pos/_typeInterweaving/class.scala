
class C[T](x: T)[U](y: U){
    def pair: (T,U) = (x,y)
    def first = x
    def second = y
}