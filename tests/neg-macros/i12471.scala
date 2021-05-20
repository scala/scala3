import scala.quoted.*

def impl1[A](using t: Type[A])(using q: Quotes) =
  '{ Type.of[A] ; ??? } // error

def impl2[A](using t: Type[A])(using q: Quotes) =
  '{ Type.of[A](using q) ; ??? } // error
