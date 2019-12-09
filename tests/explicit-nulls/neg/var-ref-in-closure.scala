// Test that we don't track variables which is refered in another closure.

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
      val a: String = x // error: x exists in closure, no longer tackable
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
      val a: String = x // error: x exists in closure, no longer tackable
    }
  }


  locally {
    var x: String|Null = ???
    lazy val y = {
      if (x != null) {
        val a: String = x // error: x exists in closure, no longer tackable
      }
      x
    }
  }

  locally {
    var x: String|Null = ???
    def y = {
      if (x != null) {
        val a: String = x // error: x exists in closure, no longer tackable
      }
      x
    }
  }

  lazy val lazyblock = {
    var x: String|Null = ???
    lazy val y = {
      if (x != null) {
        // The enclosingMethods of x definition and x reference hare are same
        val a: String = x // error: x exists in closure, no longer tackable
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
      val a: String = x // error: x exists in closure, no longer tackable
    }
  }

  locally {
    var x: String|Null = ???
    val y: F = new F {
      def get() = {
        if (x != null) {
          val a: String = x // error: x exists in closure, no longer tackable
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
      val a: String = x // error: x exists in closure, no longer tackable
    }
  }

  // TODO: not working now
  // locally {
  //   var x: String|Null = ???
  //   val y: F = f {
  //     if (x != null) {
  //       val a: String = x // err: x exists in closure, no longer tackable
  //     }
  //     x
  //   }
  // }

  locally {
    var x: String|Null = ???
    val y: String => String|Null = s => {
      if (x != null) {
        val a: String = x // error: x exists in closure, no longer tackable
      }
      x
    }
  }
}
