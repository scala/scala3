object Test:
  var x = 0
  type FreshContext = String^
  x += 1

  inline def ctx(using c: String) = c
