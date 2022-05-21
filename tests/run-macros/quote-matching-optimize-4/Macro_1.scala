import scala.quoted.*

object Macro {

  inline def optimize[T](inline x: List[T]): List[T] = ${ Macro.impl[T]('x) }

  def impl[T: Type](x: Expr[List[T]])(using Quotes): Expr[List[T]] = {
    val res = optimize(x)
    '{
      val result = $res
      val originalCode = ${Expr(x.show)}
      val optimizeCode = ${Expr(res.show)}
      println("Original: " + originalCode)
      println("Optimized: " + optimizeCode)
      println("Result: " + result)
      println()
      result
    }
  }

  def optimize[T: Type](x: Expr[List[T]])(using Quotes): Expr[List[T]] = x match {
    case '{ ($ls: List[T]).filter($f).filter($g) } =>
      optimize('{ $ls.filter(x => $f(x) && $g(x)) })

    case '{ type u; type v; ($ls: List[`u`]).map($f: (`u` => `v`)).map($g: (`v` => T)) } =>
      optimize('{ $ls.map(x => $g($f(x))) })

    case _ => x
  }
}

