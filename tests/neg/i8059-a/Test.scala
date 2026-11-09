import reflect.ClassTag

package p {
  
  class X {
    override def toString() = "p.X"
  }

  package q {
    object Test {
      def f = implicitly[ClassTag[X]] // error
    }
  }

  object Test {
    def main(args: Array[String]) = println {
      p.q.Test.f
    }
  }
}
