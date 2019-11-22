object Macros {
  inline def q : Int = ${ '[ Int ] } // error
  val x : Int = 1 + q // error

  transparent inline def q2: Int = ${ '[ Int ] } // error
  val y : Int = 1 + q2 // error
}
