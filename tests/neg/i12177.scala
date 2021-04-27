object Test1 {
  inline val v = null // error
  inline def d = null
  inline val u = () // error
  inline def e = ()
  inline val u2 = { println(); () } // error
  inline val u3 = { } // error
}
