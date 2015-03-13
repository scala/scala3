package test

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Types.Type

object DottyTypeStealer {
  def stealType(source: String, typeStrings: String*): (Context, List[Type]) = {
    val dummyName = "x_x_x"
    val vals = typeStrings.zipWithIndex.map{case (s, x)=> s"val ${dummyName}$x: $s = ???"}.mkString("\n")
    val gatheredSource = s" ${source}\n object A$dummyName {$vals}"
    var scontext : Context = null
    var tp: List[Type] = null
    new DottyTest().checkCompile("frontend",gatheredSource) {
      (tree, context) =>
        implicit val ctx = context
        val findValDef: (List[ValDef], tpd.Tree) => List[ValDef] =
          (acc , tree) =>  { tree match {
          case t: ValDef if t.name.startsWith(dummyName.toTermName) => t :: acc
          case _ => acc
        }
      }
      val d = new DeepFolder[List[ValDef]](findValDef).foldOver(Nil, tree)
      tp = d.map(_.tpe.widen).reverse
      scontext = context
    }
    (scontext, tp)
  }
}
