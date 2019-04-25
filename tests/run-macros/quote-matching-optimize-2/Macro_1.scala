import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

object Macro {

  inline def optimize[T](x: => T): Any = ${ Macro.impl('x) }

  def impl[T: Type](x: Expr[T]) given Reflection: Expr[Any] = {
    val reflect = the[Reflection]
    import reflect._ // TODO remove

    def optimize(x: Expr[Any]): Expr[Any] = x match {
      case '{ ($ls: List[$t]).filter($f).filter($g) } =>
        optimize('{ $ls.filter(x => ${f('x)} && ${g('x)}) })

      case '{ type $u; type $v; ($ls: List[$t]).map[`$u`, List[`$u`]]($f).map[`$v`, List[`$v`]]($g) } =>
        optimize('{ $ls.map(x => ${g(f('x))}) })

      case '{ ($ls: List[$t]).filter($f).foreach[$u]($g) } =>
        optimize('{ $ls.foreach[Any](x => if (${f('x)}) ${g('x)} else ()) })

      case _ => x
    }

    val res = optimize(x)

    '{
      val result = $res
      val originalCode = ${x.show}
      val optimizeCode = ${res.show}
      println("Original: " + originalCode)
      println("Optimized: " + optimizeCode)
      println("Result: " + result)
      println()
    }
  }

}
