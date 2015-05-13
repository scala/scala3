import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object A {
  object B {
    val c = ()
  }
}

object Test extends dotty.runtime.LegacyApp {
  import A.{B => X}
  import A.B.{c => y}
  import X.{c => z}

  val expr = reify (
    X.c, y, z
  )

  println(expr.eval)
}
