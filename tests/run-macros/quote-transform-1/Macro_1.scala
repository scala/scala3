import scala.quoted._
import scala.quoted.autolift._

import scala.tasty.Reflection

object Macro {

  inline def optimize[T](x: => T): Unit = ${ Macro.impl('x) }

  def impl[T: Type](x: Expr[T]) given Reflection: Expr[Unit] = {
    val reflect = the[Reflection]
    import reflect._ // TODO remove

    // Naive oprimization to test extractors (should lift into DSL, optimize and then generate code)
    val res = x.transform(
      ExprTransformer('[List[Int]]) {
        case e @ '{ ($ls: List[Int]).filter($f).filter($g) } =>
          '{ $ls.filter(x => ${f('x)} && ${g('x)}) }
      },
      ExprTransformer('[List[String]]) {
        case e @ '{ ($ls: List[Int]).map[Int, List[Int]]($f).map[String, List[String]]($g) } =>
          '{ $ls.map(x => ${g(f('x))}) }
      },
      ExprTransformer('[Unit]) {
        case e @ '{ ($ls: List[Int]).filter($f).foreach[Unit]($g) } =>
          '{ $ls.foreach(x => if (${f('x)}) ${g('x)} else ()) }

        case e @ '{ ($ls: List[Int]).filter($f).foreach($g) } =>
          '{ $ls.foreach(x => if(${f('x)}) ${g('x)} else ()) }
      }
    )
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
