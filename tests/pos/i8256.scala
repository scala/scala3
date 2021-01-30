val a = 1

  trait Test[A]
  object Test {
    implicit def ev[A] : Test[A] = new Test[A]{}
  }

  val fetch = implicitly[Test[1]]

@main def tester() : Unit = {}