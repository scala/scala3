package dotty.tools
package dotc
package parsing

import org.junit.Test
import org.junit.Assert._

import ast.untpd.modsDeco
import ast.untpd._
import ast.{ Trees => d }
import Parsers.Parser
import util.SourceFile
import core.Contexts._
import core.Flags

object ModifiersParsingTest {
  implicit val ctx: Context = (new ContextBase).initialCtx

  implicit def parse(code: String): Tree = {
    val (_, stats) = new Parser(new SourceFile("<meta>", code)).templateStatSeq()
    stats match { case List(stat) => stat; case stats => Thicket(stats) }
  }

  implicit class TreeDeco(val code: Tree) extends AnyVal {
    def firstConstrValDef: ValDef = code match {
      case d.TypeDef(_, d.Template(constr, _, _, _)) =>
        constr.vparamss.head.head
    }

    def firstTypeParam: TypeDef = code match {
      case d.TypeDef(_, d.Template(constr, _, _, _)) =>
        constr.tparams.head
    }

    def defParam(i: Int): ValDef = code match {
      case d.DefDef(_, _, vparamss, _, _) =>
        vparamss.head.toArray.apply(i)
    }

    def defParam(i: Int, j: Int): ValDef = code match {
      case d.DefDef(_, _, vparamss, _, _) =>
        vparamss.toArray.apply(i).toArray.apply(j)
    }

    def funParam(i: Int): Tree = code match {
      case Function(params, _) =>
        params.toArray.apply(i)
    }

    def field(i: Int): Tree = code match {
      case d.TypeDef(_, t: Template) =>
        t.body.toArray.apply(i)
    }

    def field(name: String): Tree = code match {
      case d.TypeDef(_, t: Template) =>
        t.body.find({
          case m: MemberDef => m.name.show == name
          case _ => false
        }).get
    }

    def stat(i: Int): Tree = code match {
      case d.Block(stats, expr) =>
        if (i < stats.length) stats.toArray.apply(i)
        else expr
    }

    def modifiers: List[Mod] = code match {
      case t: MemberDef => t.mods.mods
    }
  }
}


class ModifiersParsingTest {
  import ModifiersParsingTest._


  @Test def valDef = {
    var source: Tree = "class A(var a: Int)"
    assert(source.firstConstrValDef.modifiers == List(Mod.Var()))

    source = "class A(val a: Int)"
    assert(source.firstConstrValDef.modifiers == List())

    source = "class A(private val a: Int)"
    assert(source.firstConstrValDef.modifiers == List(Mod.Private()))

    source = "class A(protected var a: Int)"
    assert(source.firstConstrValDef.modifiers == List(Mod.Protected(), Mod.Var()))

    source = "class A(protected implicit val a: Int)"
    assert(source.firstConstrValDef.modifiers == List(Mod.Protected(), Mod.Implicit()))

    source = "class A[T]"
    assert(source.firstTypeParam.modifiers == List())
  }

  @Test def typeDef = {
    var source: Tree = "class A"
    assert(source.modifiers == List())

    source = "sealed class A"
    assert(source.modifiers == List(Mod.Sealed()))

    source = "implicit class A"
    assert(source.modifiers == List(Mod.Implicit()))

    source = "abstract sealed class A"
    assert(source.modifiers == List(Mod.Abstract(), Mod.Sealed()))
  }

  @Test def fieldDef = {
    val source: Tree =
      """
        | class A {
        |   lazy var a = ???
        |   lazy private val b = ???
        |   final val c = ???
        |
        |   abstract override def f: Boolean
        |   transparent def g(n: Int) = ???
        | }
      """.stripMargin

    assert(source.field("a").modifiers == List(Mod.Lazy(), Mod.Var()))
    assert(source.field("b").modifiers == List(Mod.Lazy(), Mod.Private()))
    assert(source.field("c").modifiers == List(Mod.Final()))
    assert(source.field("f").modifiers == List(Mod.Abstract(), Mod.Override()))
    assert(source.field("g").modifiers == List(Mod.Transparent()))
  }

  @Test def paramDef = {
    var source: Tree = "def f(transparent a: Int) = ???"
    assert(source.defParam(0).modifiers == List(Mod.Transparent()))

    source = "def f(implicit a: Int, b: Int) = ???"
    assert(source.defParam(0).modifiers == List(Mod.Implicit()))
    assert(source.defParam(1).modifiers == List(Mod.Implicit()))

    source = "def f(x: Int, y: Int)(implicit a: Int, b: Int) = ???"
    assert(source.defParam(0, 0).modifiers == List())
    assert(source.defParam(1, 0).modifiers == List(Mod.Implicit()))
  }

  @Test def blockDef = {
    var source: Tree = "implicit val x : A = ???"
    assert(source.modifiers == List(Mod.Implicit()))

    source = "implicit var x : A = ???"
    assert(source.modifiers == List(Mod.Implicit(), Mod.Var()))

    source = "{ implicit var x : A = ??? }"
    assert(source.stat(0).modifiers == List(Mod.Implicit(), Mod.Var()))

    source = "{ implicit x => x * x }"
    assert(source.stat(0).funParam(0).modifiers == List(Mod.Implicit()))
  }
}
