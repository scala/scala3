import scala.quoted._

object Macro {

  inline def optimize[T](inline x: T): Any = ${ Macro.impl('x) }

  def impl[T](using s: Scope)(x: s.Expr[T])(using s.Type[T]): s.Expr[Any] = {

    def optimize(x: s.Expr[Any]): s.Expr[Any] = x match {
      case '{ ($ls: List[$t]).filter($f).filter($g) } =>
        optimize('{ $ls.filter(x => $f(x) && $g(x)) })

      case '{ type $uu; type $vv; ($ls: List[$tt]).map[`$uu`]($f).map[String]($g) } =>
        optimize('{ $ls.map(x => $g($f(x))) })

      case '{ ($ls: List[$t]).filter($f).foreach[$u]($g) } =>
        optimize('{ $ls.foreach[Any](x => if ($f(x)) $g(x) else ()) })

      case _ => x
    }

    val res = optimize(x)

    '{
      val result = $res
      val originalCode = ${Expr(x.show)}
      val optimizeCode = ${Expr(res.show)}
      println("Original: " + originalCode)
      println("Optimized: " + optimizeCode)
      println("Result: " + result)
      println()
    }
  }

}
