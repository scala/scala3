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
 * @param kind the kind of type we are inspecting [`rhs`, `method`, `class`, `type`]
 * @param source top level Scala definitions, e.g. `"class O { type X }"`
 * @param typeStrings Scala type signatures, e.g. `"O#X"`
 *
 * @syntax markdown
 */
@main def printTypes(source: String, kind: String, typeStrings: String*) = {
  val k = DottyTypeStealer.Kind.lookup(kind)
  val (_, tpes) = DottyTypeStealer.stealType(source, k, typeStrings*)
  tpes.foreach(t => println(s"$t [${t.getClass}]"))
}

object DottyTypeStealer extends DottyTest {

  enum Kind:
    case `rhs`, `method`, `class`, `type`

    def format(name: String, arg: String) = this match
      case `rhs`    => s"val $name: $arg = ???"
      case `method` => s"def $name $arg = ???"
      case `class`  => s"class $name $arg"
      case `type`   => s"type $name $arg"

  object Kind:

    def lookup(kind: String): Kind =
      values.find(_.productPrefix == kind).getOrElse {
        println(s"unknown kind `$kind`, assuming `$rhs`")
        rhs
      }

  end Kind


  def stealType(source: String, kind: Kind, typeStrings: String*): (Context, List[Type]) = {
    val dummyName = "x_x_x"
    val vals = typeStrings.zipWithIndex.map{case (s, x) => kind.format(dummyName + x, s) }.mkString("\n")
    val gatheredSource = s" ${source}\n object A$dummyName {$vals}"
    var scontext : Context = null
    var tp: List[Type] = null
    checkCompile("typer", gatheredSource) {
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
        tp = d.map(_.symbol.info).reverse
        scontext = context
    }
    (scontext, tp)
  }
}
