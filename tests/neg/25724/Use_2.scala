package macrotest
import scala.quoted.*

inline def deepNest(inline depth: Int): Int = ${ deepNestImpl('depth) }

@main def test(): Unit =
  println(deepNest(1230)) // error
