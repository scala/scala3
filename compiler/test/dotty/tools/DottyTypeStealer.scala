package dotty.tools

import dotc.ast.tpd
import dotc.core.Names._
import dotc.ast.tpd._
import dotc.core.Contexts.{Context, atPhase}
import dotty.tools.dotc.core.Phases.{typerPhase, erasurePhase}
import dotc.core.Symbols.Symbol
import dotc.core.Decorators._
import dotc.core.Types.Type

import scala.util.CommandLineParser.FromString

/**Pass a string representing a Scala source file,
 * and then some type signatures referencing prior definitions.
 *
 * The type signatures will then be printed as raw data structures.
 *
 * @param source top level Scala definitions, e.g. `"class O { type X }"`
 * @param kind the kind of type we are inspecting [`rhs`, `method`, `class`, `type`]
 * @param typeStrings Scala type signatures, e.g. `"O#X"`
 *
 * @syntax markdown
 */
@main def printTypes(source: String, kind: DottyTypeStealer.Kind, typeStrings: String*) = {
  val (_, tpes) = DottyTypeStealer.stealType(source, kind, typeStrings*)
  tpes.foreach(t => println(s"$t [${t.getClass}]"))
}

/**Pass a string representing a Scala source file,
 * and then some type signatures referencing prior definitions.
 *
 * The type signatures will then be printed comparing between phase
 * `typer` where types are as Scala understands them and phase `erasure`,
 * which models the JVM types.
 *
 * @param source top level Scala definitions, e.g. `"class O { type X }"`
 * @param kind the kind of type we are inspecting [`rhs`, `method`, `class`, `type`]
 * @param typeStrings Scala type signatures, e.g. `"O#X"`
 *
 * @syntax markdown
 */
@main def printTypesAndErasure(source: String, kind: DottyTypeStealer.Kind, typeStrings: String*): Unit =
  val (ictx, vdefs) = DottyTypeStealer.stealMember("erasure", source, kind, typeStrings*)

  given Context = ictx

  for vdef <- vdefs do
    println(i"info @ typer   => ${atPhase(typerPhase.next)(vdef.info)}")
    println(i"info @ erasure => ${atPhase(erasurePhase.next)(vdef.info)}")
end printTypesAndErasure

object DottyTypeStealer extends DottyTest {

  given FromString[Kind] = kind =>
    if kind == "" then
      println(s"assuming kind `${Kind.rhs}`")
      Kind.rhs
    else
      Kind.valueOf(kind)

  enum Kind:
    case `rhs`, `method`, `class`, `type`

    def format(name: String, arg: String) = this match
      case `rhs`    => s"val $name: $arg = ???"
      case `method` => s"def $name $arg = ???"
      case `class`  => s"class $name $arg"
      case `type`   => s"type $name $arg"

  def stealType(source: String, kind: Kind, typeStrings: String*): (Context, List[Type]) = {
    val (scontext, members) = stealMember("typer", source, kind, typeStrings*)
    given Context = scontext
    (scontext, members.map(_.info))
  }

  def stealMember(lastPhase: String, source: String, kind: Kind, typeStrings: String*): (Context, List[Symbol]) = {
    val dummyName = "x_x_x"
    val vals = typeStrings.zipWithIndex.map{case (s, x) => kind.format(dummyName + x, s) }.mkString("\n")
    val gatheredSource = s" ${source}\n object A$dummyName {$vals}"
    var scontext : Context = null
    var members: List[Symbol] = null
    checkCompile(lastPhase, gatheredSource) {
      (tree, context) =>
        given Context = context
        val findMemberDef: (List[MemberDef], tpd.Tree) => List[MemberDef] =
          (acc , tree) =>  tree match {
            case t: DefDef if t.name.startsWith(dummyName) => t :: acc
            case t: ValDef if t.name.startsWith(dummyName) => t :: acc
            case t: TypeDef if t.name.startsWith(dummyName) => t :: acc
            case _ => acc
          }
        val d = new DeepFolder[List[MemberDef]](findMemberDef).foldOver(Nil, tree)
        members = d.map(_.symbol).reverse
        scontext = context
    }
    (scontext, members)
  }
}
