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
    var x: String|Null = "???"
    lazy val y = {
      if (x != null) {
        val a: String = x // error: x exists in closure, no longer tackable
      }
      x
    }
  }

  locally {
    var x: String|Null = "???"
    def y = {
      if (x != null) {
        val a: String = x // error: x exists in closure, no longer tackable
      }
      x
    }
  }

  lazy val lazyblock = {
    var x: String|Null = "???"
    lazy val y = {
      if (x != null) {
        // The enclosingMethods of x definition and x reference hare are same
        val a: String = x // error: x exists in closure, no longer tackable
      }
      x
    }
  }
}

