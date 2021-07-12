package dotty.tools

import dotc.ast.tpd
import dotc.core.Names._
import dotc.ast.tpd._
import dotc.core.Contexts.Context
import dotc.core.Decorators._
import dotc.core.Types.Type

/**Pass a string representing a Scala source file,
 * and then some type signatures referencing prior definitions.
 *
 * The type signatures will then be printed (singleton types
 * are widened.)
 *
 * @param source top level Scala definitions, e.g. `"class O { type X }"`
 * @param typeStrings Scala type signatures, e.g. `"O#X"`
 *
 * @syntax markdown
 */
@main def printTypes(source: String, typeStrings: String*) = {
  val (_, tpes) = DottyTypeStealer.stealType(source, typeStrings*)
  tpes.foreach(println)
}

object DottyTypeStealer extends DottyTest {
  def stealType(source: String, typeStrings: String*): (Context, List[Type]) = {
    val dummyName = "x_x_x"
    val vals = typeStrings.zipWithIndex.map{case (s, x)=> s"val ${dummyName}$x: $s = ???"}.mkString("\n")
    val gatheredSource = s" ${source}\n object A$dummyName {$vals}"
    var scontext : Context = null
    var tp: List[Type] = null
    checkCompile("typer", gatheredSource) {
      (tree, context) =>
        given Context = context
        val findValDef: (List[ValDef], tpd.Tree) => List[ValDef] =
          (acc , tree) =>  tree match {
            case t: ValDef if t.name.startsWith(dummyName) => t :: acc
            case _ => acc
          }
        val d = new DeepFolder[List[ValDef]](findValDef).foldOver(Nil, tree)
        tp = d.map(_.tpe.widen).reverse
        scontext = context
    }
    (scontext, tp)
  }
}
