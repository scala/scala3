trait A {
    type Type[X,]  // error
    def a[X]: Type[X,]  // error
}
class B extends A {
    type Type[X]
    var a = 1
}