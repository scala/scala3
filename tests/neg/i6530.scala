object Macros {
  inline def q : Int = ${ '[ Int ] } // error
  val x : Int = 1 + q
}
