class A:
  val a = 2

  class B:
    val b = 3

    trait T:
      def t = a + b

  val bb = B()

  class C extends bb.T:
    def result = a + t

@main def Test =
  val a = A()
  val c = a.C()
  assert(c.result == 7)
