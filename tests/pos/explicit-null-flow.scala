
// Flow-sensitive type inference
class Foo {

  class Bar {
    val s: String = ???
  }

  // Basic
  val b: Bar|Null = ???
  if (b != null) {
    val s = b.s   // ok: type of `b` inferred as `Bar`
    val s2: Bar = b
  } else {
  }

  //  Not stable
  var b2: Bar|Null = ???
  if (b2 != null) {
  }

  class Bar2 {
    val x: Bar2|Null = ???
  }

  // Nested and selection
  val bar2: Bar2|Null = ???
  if (bar2 != null) {
    if (bar2.x != null) {
      if (bar2.x.x != null) {
        if (bar2.x.x.x != null) {
          val b2: Bar2 = bar2.x.x.x
        }
        val b2: Bar2 = bar2.x.x
      }
      val b2: Bar2 = bar2.x
    }
    val b2: Bar2 = bar2
  }

  // If-then-else and equality
  val s: String|Null = ???
  if (s == null) {
  } else {
    val len: Int = s.length
    val len2 = s.length
  }

  // Elseif
  val s1: String|Null = ???
  val s2: String|Null = ???
  val s3: String|Null = ???
  if (s1 != null) {
    val len = s1.length
  } else if (s2 != null) {
    val len = s2.length
  } else if (s3 != null) {
    val len = s3.length
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

  // Common idioms
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

  // Basic negation
  if (!(s1 != null)) {
  } else {
    val len = s1.length
  }

  if (!(!(!(!(s1 != null))))) {
    val len1 = s1.length
  }

  // Parens
  if ((((s1 == null))) || s2 == null) {
  } else {
    val len1 = s1.length
    val len2 = s2.length
  }

  // Operator precedence
  if (s1 != null || s2 != null && s3 != null) {
  }

  if (s1 != null && s2 != null || s3 != null) {
  }

  if (s1 != null && (s2 != null || s3 != null)) {
    val len1 = s1.length
  }
}
