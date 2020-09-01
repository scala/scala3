object Macros {
  inline def q : Int = ${ Type[ Int ] } // error
  val x : Int = 1 + q
}