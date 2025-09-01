 class STR
 object Test:
  var x = 0
  type FreshContext = STR^
  x += 1

  inline def ctx(using c: STR) = c

  val y: STR^ -> Unit = ???
  val z: STR^ ?-> Unit = ???
