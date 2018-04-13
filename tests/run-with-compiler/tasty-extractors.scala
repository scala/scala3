
import scala.quoted._

import scala.tasty.constants.Constant
import scala.tasty.names
import scala.tasty.patterns
import scala.tasty.statements
import scala.tasty.terms
import scala.tasty.typetrees
import scala.tasty.types

import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Toolbox._

import scala.tasty.modifiers.Modifier

object Test {
  def main(args: Array[String]): Unit = {
    for (q <- quotes) {
      val tasty = toTasty(q)
      println(tasty)
      println(tasty.tpe)
      println()
    }
  }

  def quotes: List[Expr[_]] = List(
    '(true),
    '(1),
    '(2L),
    '(2d),
    '("abc"),
    '(println("abc")),
    '(8: Int),
    '{ 1; 2; 3 },
    '(if (true: Boolean) 1 else 2),
    '("a" match { case "a" => () }),
    '("b" match { case n => () }),
    '("c" match { case n: String => () }),
    '("e" match { case _ => () }),
    '("f" match { case _: String => () }),
    '("g" match { case _: String | _: Int => () }),
    '("h" match { case _ if false => () }),
    '{ val a = "o"; "i" match { case a => () } },
    '(Option(4) match { case Some(a) => a; case None => 1 }),
    '(Nil match { case List(a, b, c) => }),
    '(try 1 catch { case _ => }),
    '(try 2 finally ()),
    '(try 3 catch { case _ => } finally ()),
    '("a" == "b"),
    '(new Object),
    '(Int.box(x = 9)),
    '(Ordering.apply[Int]),
    '{ val a: Int = 3 },
    '{ lazy val b: Int = 3 },
    '{ def f1: Int = 3 },
    '{ def f2: Int = return 4 },
    '{ def f3(i: Int): Int = i },
    '{ def f4(i: Int)(j: Int): Int = i + j },
    '{ def f5(i: Int = 9): Int = i },
    '{ def f6[T](x: T): T = x },
    '{ def f7[T](x: T): x.type = x },
    '{ def f8(i: Int*): Int = 9; f8(1, 2, 3) },
    '{ def f9(i: => Int): Int = i },
    '{ var x = 1; x = 2 },
    '((x: Int) => x),
    '(???),
    '(1: 1),
    '(1: Int),
    '(Nil: List[Int]),
    '(1: Int & Int),
    '(1: Int | String),
    '{ import scala.collection.mutable; 1 },
    '{ import scala.collection.{mutable, immutable}; 2 },
    '{ import scala.collection.{mutable => mut}; 3 },
    '{ import scala.collection.{mutable => _}; 4 },
    '{ class Foo },
    '{ object Foo },
    '{ type Foo },
    '{ type Foo = Int },
    '{ type Foo >: Null <: Object },
    '{ class Foo { @volatile var a = 0 } },
    '{ class Foo { final def a = 0 } }, // FIXME modifier not printed
//    '{ case class Foo() },
    '{ class Foo1(a: Int) },
    '{ class Foo2(val b: Int) },
    '{ class Foo3(a: Int = 5) },
    '{ class Foo4(a: Int)(b: Int) },
    '{ class Foo5(a: Int)(b: Int = a) },
    '{ class Foo6(a: Int)(b: a.type) },
//    '{ class Foo7(a: Int) { def this() = this(6) } },
    '{ class Foo8 { println(0) } },
    '{ class Foo10 { val a = 9 } },
    '{ class Foo11 { var a = 10 } },
    '{ class Foo12 { lazy val a = 11 } },
    '{ class Foo; class Bar extends Foo },
//    '{ trait Foo; class Bar extends Foo },
    '{ class Foo(i: Int); class Bar extends Foo(1) },
    '{ class Foo { type X = Int }; def f(a: Foo): a.X = ??? },
    '{ class Foo { type X }; def f(a: Foo { type X = Int }): a.X = ??? },

  )

  class TreeTraverser {

    def traverse(arg: statements.TopLevelStatement): Unit = {
      import statements._
      arg match {
        case arg: terms.Term =>
          traverse(arg)
        case arg: Definition =>
          traverse(arg)
        case Package(pkg, body) =>
          traverse(pkg)
          body.map(traverse)
        case Import(expr, selectors) =>
          traverse(expr)
          selectors.map(traverse)
        case _ =>
      }
    }

    def traverse(arg: statements.Definition): Unit = {
      import statements._
      arg match {
        case ValDef(name, tpt, rhs, mods) =>
          traverse(name)
          traverse(tpt)
          rhs.foreach(traverse)
          mods.foreach(traverse)
        case DefDef(name, typeParams, paramss, returnTpt, rhs, mods) =>
          traverse(name)
          typeParams.map(traverse)
          paramss.map(_.map(traverse))
          traverse(returnTpt)
          rhs.map(traverse)
          mods.map(traverse)
        case TypeDef(name, rhs, mods) =>
          traverse(name)
          traverse(rhs)
          mods.map(traverse)
        case ClassDef(name, contraverseuctor, parents, self, body, mods) =>
          traverse(name)
          traverse(contraverseuctor)
          parents.map(traverse)
          self.map(traverse)
          body.map(traverse)
          mods.map(traverse)
        case _ =>
      }
    }

    def traverse(arg: terms.Term): Unit = {
      import terms._
      arg match {
        case Ident(name) =>
          traverse(name)
        case Select(qual, name) =>
          traverse(qual)
          traverse(name)
        case Literal(const) =>
          traverse(const)
        case New(tpt) =>
          traverse(tpt)
        case NamedArg(name, arg) =>
          traverse(name)
          traverse(arg)
        case Apply(fn, args) =>
          traverse(fn)
          args.map(traverse)
        case TypeApply(fn, args) =>
          traverse(fn)
          args.map(traverse)
        case Super(qual, mixin) =>
          traverse(qual)
          mixin.map(traverse)
        case Typed(expr, tpt) =>
          traverse(expr)
          traverse(tpt)
        case Assign(lhs, rhs) =>
          traverse(lhs)
          traverse(rhs)
        case Block(stats, expr) =>
          stats.map(traverse)
          traverse(expr)
        case Lambda(meth, tpt) =>
          traverse(meth)
          tpt.map(traverse)
        case If(cond, thenp, elsep) =>
          traverse(cond)
          traverse(thenp)
          traverse(elsep)
        case Match(selector, cases) =>
          traverse(selector)
          cases.map(traverse)
        case Try(body, catches, finalizer) =>
          traverse(body)
          catches.map(traverse)
          finalizer.map(traverse)
        case Return(expr) =>
          traverse(expr)
        case Repeated(args) =>
          args.map(traverse)
        case _ =>
      }
    }

    def traverse(arg: patterns.CaseDef): Unit = {
      import patterns.CaseDef
      arg match {
        case CaseDef(pat, guard, body) =>
          traverse(pat)
          guard.foreach(traverse)
          traverse(body)
        case _ =>
      }
    }

    def traverse(arg: patterns.Pattern): Unit = {
      import patterns._
      arg match {
        case Value(v) =>
          traverse(v)
        case Bind(name, body) =>
          traverse(name)
          traverse(body)
        case Unapply(fun, implicits, patterns) =>
          traverse(fun)
          implicits.map(traverse)
          patterns.map(traverse)
        case Alternative(patterns) =>
          patterns.map(traverse)
        case TypeTest(tpt) =>
          traverse(tpt)
        case _ =>
      }
    }

    def traverse(arg: names.TermName): Unit = {
      import names._
      arg match {
        case Simple(name) =>
        case _ => ???
      }
    }

    def traverse(arg: names.PossiblySignedName): Unit = {
      arg match {
        case _ => ???
      }
    }

    def traverse(arg: typetrees.TypeTree): Unit = {
      import typetrees._
      arg match {
        case Synthetic() =>
        case Ident(name) => traverse(name)
        case Select(qual, name) =>
          traverse(qual)
          traverse(name)
        case Singleton(ref) => traverse(ref)
        //      case Refined(tpt, refinements) =>
        //        traverse(ref)
        //        refinements.foreach(traverse)
        case Applied(tycon, args) =>
          traverse(tycon)
          args.map(traverse)
        case TypeBounds(lo, hi) =>
          traverse(lo)
          traverse(hi)
        case Annotated(arg, annot) =>
          traverse(arg)
          traverse(annot)
        case And(left, right) =>
          traverse(left)
          traverse(right)
        case Or(left, right) =>
          traverse(left)
          traverse(right)
        case ByName(tpt) =>
          traverse(tpt)
        case _ =>
      }
    }

    def traverse(arg: names.TypeName): Unit = arg match {
      case _ => arg.toString // TODO
      case _ =>
    }

    def traverse(arg: scala.tasty.Id): Unit = ()

    def traverse(arg: Modifier): Unit = ()

    def traverse(arg: statements.Import.ImportSelector): Unit = {
      import statements.Import.ImportSelector._
      arg match {
        case Simple(id) =>
          traverse(id)
        case Rename(id1, id2) =>
          traverse(id1)
          traverse(id2)
        case Omit(id) =>
          traverse(id)
      }
    }

    def traverse(arg: Constant): Unit = ()
  }
}
