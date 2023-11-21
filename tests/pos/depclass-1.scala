class A(tracked val source: String)

class B(source: String) extends A(source)

class C(source: String) extends B(source)

val x = C("hello")
val _: A{ val source: "hello" } = x