object Test:
  var x = 0
  type FreshContext = String^
  x += 1

  inline def ctx(using c: String) = c

  val y: String^ -> Unit = ???
  val z: String^ ?-> Unit = ???
