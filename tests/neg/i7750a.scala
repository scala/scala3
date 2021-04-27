def foo: Unit = {
    val a = (x: Int, y: String) => x + + y  // error
    implicit def f[X](x: (X,  String) => String) = (z: X) => (z, null) // error
    a(1) // error
}