// Test that we don't track variables which is referred in another closure.

object VarRef {
  locally {
    var x: String|Null = ???
    val y = {
      if (x != null) {
        val _: String = x // ok: y doesn't create closure
      }
    }
    if (x != null) {
      val a: String = x // ok
    }
  }

  locally {
    var x: String|Null = ???
    var y = {
      if (x != null) {
        val _: String = x // ok: y doesn't create closure
      }
    }
    if (x != null) {
      val a: String = x // ok
    }
  }

  locally {
    var x: String|Null = ???
    lazy val y = {
      if (x != null) {
        x = null
      }
      x
    }
    if (x != null) {
      val a: String = x // error: x exists in closure, no longer trackable
    }
  }

  locally {
    var x: String|Null = ???
    def y = {
      if (x != null) {
        x = null
      }
      x
    }
    if (x != null) {
      val a: String = x // error: x exists in closure, no longer trackable
    }
  }


  locally {
    var x: String|Null = ???
    lazy val y = {
      if (x != null) {
        val a: String = x // error: x exists in closure, no longer trackable
      }
      x
    }
  }

  locally {
    var x: String|Null = ???
    def y = {
      if (x != null) {
        val a: String = x // error: x exists in closure, no longer trackable
      }
      x
    }
  }

  lazy val lazyblock = {
    var x: String|Null = ???
    lazy val y = {
      if (x != null) {
        // The enclosingMethods of x definition and x reference hare are same
        val a: String = x // error: x exists in closure, no longer trackable
      }
      x
    }
  }

  abstract class F {
    def get(): String | Null
  }

  locally {
    var x: String|Null = ???
    val y: F = new F {
      def get() = {
        if (x != null) x = null
        x
      }
    }
    if (x != null) {
      val a: String = x // error: x exists in closure, no longer trackable
    }
  }

  locally {
    var x: String|Null = ???
    val y: F = new F {
      def get() = {
        if (x != null) {
          val a: String = x // error: x exists in closure, no longer trackable
        }
        x
      }
    }
  }

  def f(x: => String | Null): F = new F {
    def get() = x
  }

  locally {
    var x: String|Null = ???
    val y: F = f {
      if (x != null) {
        x = null
      }
      x
    }
    if (x != null) {
      val a: String = x // error: x exists in closure, no longer trackable
    }
  }

  // TODO: not working now
  // locally {
  //   var x: String|Null = ???
  //   val y: F = f {
  //     if (x != null) {
  //       val a: String = x // err: x exists in closure, no longer trackable
  //     }
  //     x
  //   }
  // }

  locally {
    var x: String|Null = ???
    val y: String => String|Null = s => {
      if (x != null) {
        val a: String = x // error: x exists in closure, no longer trackable
      }
      x
    }
  }

  locally {
    val x: String|Null = ???
    if (x != null) {
      def f = {
        val y: String = x // ok, x is a value definition
        y
      }
    }
  }

  locally {
    var x: String|Null = ???
    if (x != null) {
      def f = {
        val y: String = x // error: the use of x is out of order
        y
      }
    }
  }
}
