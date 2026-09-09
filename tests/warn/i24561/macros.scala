import ext.Reader
import scala.quoted.*

object macros:
  inline def demo: Unit = ${ demoImpl }

  def demoImpl(
    using Quotes
  ): Expr[Unit] =
    '{
      //val r: ext.Reader[Option, String, String] = ???
      val r: Reader[Option, String, String] = ???
      ()
    }
