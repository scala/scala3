object O:

  class C(x: Int):
    def bar = x
    def baz(y: Int) = x + y
    val bam = x * x
    def :: (y: Int) = x - y
    class D

  val c1 = C(1)

  object O1:
    extension (x: Int)
      export c1.*   // error

  object O2:
    extension (x: Int)
      private def cm = new C(x)
      export cm.{bar, D}  // error
      export this.cm.baz  // error

  object O3:
    extension (x: Int)
      export missing.*  // error

  object O4:
    extension (x: Int)
      export cm.*   // error

  object O5:
    extension (x: Int)
      private def cm(y: C) = C
      export cm.*   // error

  {
    extension (x: Int)
      private def cm = new C(x)
      export cm.*   // error
  }

