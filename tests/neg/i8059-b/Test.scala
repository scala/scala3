import reflect.ClassTag

package p {
  
  class X {
    override def toString() = "p.X"
  }

  package q {
    import r.X
    object Test {
      def f = implicitly[ClassTag[X]] // error
    }
  }

  package r {
    class X {
      override def toString() = "p.r.X"
    }
  }

  object Test {
    def main(args: Array[String]) = println {
      p.q.Test.f
    }
  }
}
