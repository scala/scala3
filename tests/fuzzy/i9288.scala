// AE-de752a30942c1949a413940222cd9e573e5aa6a4
class Foo {
    import annotation.static
    class Bar
    object Bar {
        @static val x = 2
    }
    import Bar._
    x
}
