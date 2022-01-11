import language.experimental.erasedDefinitions
erased class ErasedTerm

erased class <::<[-From, +To] extends ErasedTerm

erased class =::=[From, To] extends (From <::< To)

erased given [X]: (X =::= X)

extension [From](x: From)
  inline def cast[To](using From <::< To): To = x.asInstanceOf[To] // Safe cast because we know `From <:< To`


def convert[A, B](a: A)(using /*erased*/ x: A <::< B): B =
  // println(x) // error: OK because x should be erased
                // but currently x is not marked as erased which it should
  a.cast[B]


@main def App: Unit = convert[Int, Int](3) // should not be an error
