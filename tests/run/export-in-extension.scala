object O:

  class C(x: Int):
    def bar = x
    def baz(y: Int) = x + y
    val bam = x * x
    def :: (y: Int) = x - y

  extension (x: Int)
    private def cm = new C(x)
    export cm.*
    def succ: Int = x + 1
    def succ2: Int = succ + 1
    def ::: (y: Int) = x - y

@main def Test =
  import O.*
  assert(3.succ2 == 5)
  assert(3.bar == 3)
  assert(3.baz(3) == 6)
  assert(3.bam == 9)
  assert(3 :: 2 :: 10 == (10 - 2) - 3)  // same as for implicit class C
  assert(3 ::: 2 ::: 10 == 3 - (2 - 10))
