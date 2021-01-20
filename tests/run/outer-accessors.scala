class A with
  val a = 2

  class B with
    val b = 3

    trait T with
      def t = a + b

  val bb = B()

  class C extends bb.T with
    def result = a + t

@main def Test =
  val a = A()
  val c = a.C()
  assert(c.result == 7)
