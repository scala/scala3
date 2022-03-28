sealed trait Foo[A, X <: A]

type FooX[F] = F match {
  case Foo[a, x] => x
}

type MyFoo = Foo[String, "hello"]

val hello: FooX[MyFoo] = "hello"
