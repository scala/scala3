trait A {
  type T
}

object O {
  implicit def b(implicit x: A): x.T = error("")
}

class Test {
  import O._
  implicit val a: A = new A {}
  implicitly[a.T]       // works

  implicitly[a.T](b(a)) // works
}
