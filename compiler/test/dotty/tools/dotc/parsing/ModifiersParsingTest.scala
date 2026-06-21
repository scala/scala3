package dotty.tools
package dotc
package parsing

import org.junit.Test
import org.junit.Assert.*

import ast.Trees.mods
import ast.untpd.*
import ast.{ Trees => d }
import Parsers.Parser
import util.SourceFile
import core.Contexts.*
import core.Flags

object ModifiersParsingTest {
  given Context = (new ContextBase).initialCtx

  def parse(code: String): Tree = {
    val (_, stats) = new Parser(SourceFile.virtual("<meta>", code)).templateStatSeq()
    stats match { case Vector(stat) => stat; case stats => Thicket(stats) }
  }

  extension (code: Tree) {
    def firstConstrValDef: ValDef = code match {
      case d.TypeDef(_, d.Template(constr, _, _, _)) =>
        constr.termParamss.head.head
    }

    def firstTypeParam: TypeDef = code match {
      case d.TypeDef(_, d.Template(constr, _, _, _)) =>
        constr.leadingTypeParams.head
    }

    def defParam(i: Int): ValDef = code match {
      case code @ d.DefDef(_, _, _, _) =>
        code.termParamss.head.toArray.apply(i)
    }

    def defParam(i: Int, j: Int): ValDef = code match {
      case code @ d.DefDef(_, _, _, _) =>
        code.termParamss.toArray.apply(i).toArray.apply(j)
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

    def modifiers: Vector[Mod] = code match {
      case t: MemberDef => t.mods.mods
    }
  }
}


class ModifiersParsingTest {
  import ModifiersParsingTest.{_, given}


  @Test def valDef = {
    var source = parse("class A(var a: Int)")
    assert(source.firstConstrValDef.modifiers == Vector(Mod.Var()))

    source = parse("class A(val a: Int)")
    assert(source.firstConstrValDef.modifiers == Vector())

    source = parse("class A(private val a: Int)")
    assert(source.firstConstrValDef.modifiers == Vector(Mod.Private()))

    source = parse("class A(protected var a: Int)")
    assert(source.firstConstrValDef.modifiers == Vector(Mod.Protected(), Mod.Var()))

    source = parse("class A(protected implicit val a: Int)")
    assert(source.firstConstrValDef.modifiers == Vector(Mod.Protected(), Mod.Implicit()))

    source = parse("class A[T]")
    assert(source.firstTypeParam.modifiers == Vector())
  }

  @Test def typeDef = {
    var source = parse("class A")
    assert(source.modifiers == Vector())

    source = parse("sealed class A")
    assert(source.modifiers == Vector(Mod.Sealed()))

    source = parse("implicit class A")
    assert(source.modifiers == Vector(Mod.Implicit()))

    source = parse("abstract sealed class A")
    assert(source.modifiers == Vector(Mod.Abstract(), Mod.Sealed()))
  }

  @Test def fieldDef = {
    val source =
      parse("""
        | class A {
        |   lazy var a = ???
        |   lazy private val b = ???
        |   final val c = ???
        |
        |   abstract override def f: Boolean
        |   inline def g(n: Int) = ???
        | }
      """.stripMargin)

    assert(source.field("a").modifiers == Vector(Mod.Lazy(), Mod.Var()))
    assert(source.field("b").modifiers == Vector(Mod.Lazy(), Mod.Private()))
    assert(source.field("c").modifiers == Vector(Mod.Final()))
    assert(source.field("f").modifiers == Vector(Mod.Abstract(), Mod.Override()))
    assert(source.field("g").modifiers == Vector(Mod.Inline()))
  }

  @Test def paramDef = {
    var source: Tree = parse("def f(inline a: Int) = ???")
    assert(source.defParam(0).modifiers == Vector(Mod.Inline()))

    source = parse("def f(implicit a: Int, b: Int) = ???")
    assert(source.defParam(0).modifiers == Vector(Mod.Implicit()))
    assert(source.defParam(1).modifiers == Vector())

    source = parse("def f(x: Int, y: Int)(implicit a: Int, b: Int) = ???")
    assert(source.defParam(0, 0).modifiers == Vector())
    assert(source.defParam(1, 0).modifiers == Vector(Mod.Implicit()))
  }

  @Test def blockDef = {
    var source: Tree = parse("implicit val x : A = ???")
    assert(source.modifiers == Vector(Mod.Implicit()))

    source = parse("implicit var x : A = ???")
    assert(source.modifiers == Vector(Mod.Implicit(), Mod.Var()))

    source = parse("{ implicit var x : A = ??? }")
    assert(source.stat(0).modifiers == Vector(Mod.Implicit(), Mod.Var()))

    source = parse("{ implicit x => x * x }")
    assert(source.stat(0).funParam(0).modifiers == Vector(Mod.Implicit()))
  }
}
