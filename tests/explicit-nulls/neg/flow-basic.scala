
// Flow-sensitive type inference
class Foo {

  def basic() = {
    class Bar {
      val s: String = ???
    }

    // Basic
    val b: Bar|Null = ???
    if (b != null) {
      val s = b.s   // ok: type of `b` inferred as `Bar`
      val s2: Bar = b
    } else {
      val s = b.s   // error: `b` is `Bar|Null`
    }
    val s = b.s     // error: `b` is `Bar|Null`
  }

  def notStable() = {
    class Bar {
      var s: String = ???
    }

    var b2: Bar|Null = ???
    if (b2 != null) {
      val s = b2.s
    }
  }

  def nested() = {
    class Bar2 {
      val x: Bar2|Null = ???
    }

    val bar2: Bar2|Null = ???
    if (bar2 != null) {
      if (bar2.x != null) {
        if (bar2.x.x != null) {
          if (bar2.x.x.x != null) {
            val b2: Bar2 = bar2.x.x.x
          }
          val b2: Bar2 = bar2.x.x
          val b2err: Bar2 = bar2.x.x.x // error: expected Bar2 but got Bar2|Null
        }
        val b2: Bar2 = bar2.x
      }
      val b2: Bar2 = bar2
    }
  }

  def ifThenElse() = {
    val s: String|Null = ???
    if (s == null) {
    } else {
      val len: Int = s.length
      val len2 = s.length
    }
  }

  def elseIf() = {
    val s1: String|Null = ???
    val s2: String|Null = ???
    val s3: String|Null = ???
    if (s1 != null) {
      val len = s1.length
      val err1 = s2.length // error
      val err2 = s3.length // error
    } else if (s2 != null) {
      val len = s2.length
      val err1 = s1.length // error
      val err2 = s3.length // error
    } else if (s3 != null) {
      val len = s3.length
      val err1 = s1.length // error
      val err2 = s2.length // error
  }

    // Accumulation in  elseif
    if (s1 == null) {
    } else if (s2 == null) {
      val len = s1.length
    } else if (s3 == null) {
      val len1 = s1.length
      val len2 = s2.length
    } else {
      val len1 = s1.length
      val len2 = s2.length
      val len3 = s3.length
    }
  }

  def commonIdioms() = {
    val s1: String|Null = ???
    val s2: String|Null = ???
    val s3: String|Null = ???

    if (s1 == null || s2 == null || s3 == null) {
    } else {
      val len1: Int = s1.length
      val len2: Int = s2.length
      val len3: Int = s3.length
    }

    if (s1 != null && s2 != null && s3 != null) {
      val len1: Int = s1.length
      val len2: Int = s2.length
      val len3: Int = s3.length
    }
  }

  def basicNegation() = {
    val s1: String|Null = ???
    if (!(s1 != null)) {
      val len = s1.length // error
    } else {
      val len = s1.length
    }

    if (!(!(!(!(s1 != null))))) {
      val len1 = s1.length
    }
  }

  def parens() = {
    val s1: String|Null = ???
    val s2: String|Null = ???
    if ((((s1 == null))) || s2 == null) {
    } else {
      val len1 = s1.length
      val len2 = s2.length
    }
  }

  def operatorPrec() = {
    val s1: String|Null = ???
    val s2: String|Null = ???
    val s3: String|Null = ???

    if (s1 != null || s2 != null && s3 != null) {
      val len = s3.length // error
    }

    if (s1 != null && s2 != null || s3 != null) {
      val len1 = s1.length // error
      val len2 = s2.length // error
      val len3 = s3.length // error
    }

    if (s1 != null && (s2 != null || s3 != null)) {
      val len1 = s1.length
      val len2 = s2.length // error
      val len3 = s3.length // error
    }
  }

  def insideCond() = {
    val x: String|Null = ???
    if (x != null && x.length > 0) {
      val len = x.length
    } else {
      val len = x.length // error
    }

    if (x == null || x.length > 0) {
      val len = x.length // error
    } else {
      val len = x.length
    }

    class Rec {
      val r: Rec|Null = ???
    }

    val r: Rec|Null = ???
    if (r != null && r.r != null && (r.r.r == null || r.r.r.r == r)) {
      val err = r.r.r.r // error
    }
  }
}

