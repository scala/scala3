
import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._
import dotty.tools.dotc.tasty.Extractors._

import scala.tasty.statement._
import scala.tasty.term._
import scala.tasty.{Modifier, PossiblySignedName, TermName}


object Test {
  def main(args: Array[String]): Unit = {
    for (q <- quotes) {
      val tasty = toTasty(q)
      println(str(tasty))
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
      // TODO add unapply case
    '(try 1 catch { case _ => }),
    '(try 2 finally ()),
    '(try 3 catch { case _ => } finally ()),
    '("a" == "b"),
    '(new Object),
    '(Int.box(x = 9)),
    '(Ordering.apply[Int]),
    '{ val a: Int = 3 },
    '{ lazy val b: Int = 3 },
    '{ def c: Int = 3 },
    '{ def d: Int = return 4 },
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
  )

  def str(arg: TopLevelStatement): String = arg match {
    case arg: Term => str(arg)
    case arg: Definition => str(arg)
    case Package(pkg, body) => s"Package(${str(pkg)}, ${body.map(str)})"
    case Import(expr, selectors) => s"Import(${str(expr)}, ${selectors.map(str)})"
  }

  def str(arg: Definition): String = arg match {
    case ValDef(name, tpt, rhs, mods) => s"ValDef(${str(name)}, ${str(tpt)}, ${rhs.map(str)}, ${mods.map(str)})"
    case DefDef(name, typeParams, paramss, returnTpt, rhs, mods) =>
      s"DefDef(${str(name)}, ${typeParams.map(str)}, ${paramss.map(_.map(str))}, ${str(returnTpt)}, ${rhs.map(str)}, ${mods.map(str)})"
    case TypeDef(name, rhs, mods) => s"TypeDef(${str(name)}, ${str(rhs)}, ${mods.map(str)})"
    case ClassDef(name, constructor, parents, self, body, mods) =>
      s"ClassDef(${str(name)}, ${str(constructor)}, ${parents.map(str)}, ${self.map(str)}, ${body.map(str)}, ${mods.map(str)})"
    case _ => "###"
  }

  def str(arg: Term): String = arg match {
    case Ident(name) => s"Ident(${str(name)})"
    case Select(qual, name) => s"Select(${str(qual)}, ${str(name)})"
    case Literal(const) => s"Literal(${str(const)})"
    case New(tpt) => s"New(${str(tpt)})"
    case NamedArg(name, arg) => s"NamedArg(${str(name)}, ${str(arg)})"
    case Apply(fn, args) => s"Apply(${str(fn)}, ${args.map(str)})"
    case TypeApply(fn, args) => s"TypeApply(${str(fn)}, ${args.map(str)})"
    case Super(qual, mixin) => s"Super(${str(qual)}, ${mixin.map(str)})"
    case Typed(expr, tpt) => s"Typed(${str(expr)}, ${str(tpt)})"
    case Assign(lhs, rhs) => s"Assign(${str(lhs)}, ${str(rhs)})"
    case Block(stats, expr) => s"Block(${stats.map(str)}, ${str(expr)})"
    case Lambda(meth, tpt) => s"Lambda(${str(meth)}, ${tpt.map(str)})"
    case If(cond, thenp, elsep) => s"If(${str(cond)}, ${str(thenp)}, ${str(elsep)})"
    case Match(selector, cases) => s"Match(${str(selector)}, ${cases.map(str)})"
    case Try(body, catches, finalizer) => s"Try(${str(body)}, ${catches.map(str)}, ${finalizer.map(str)})"
    case Return(expr) => s"Return(${str(expr)})"
    case Repeated(args) => s"Repeated(${args.map(str)})"
    case _ => "###"
  }

  def str(arg: scala.tasty.pattern.CaseDef): String = {
    import scala.tasty.pattern.CaseDef
    arg match {
      case CaseDef(pat, guard, body) => s"CaseDef(${str(pat)}, ${guard.map(str)}, ${str(body)})"
      case _ => "###"
    }
  }

  def str(arg: scala.tasty.pattern.Pattern): String = {
    import scala.tasty.pattern._
    arg match {
      case Value(v) => s"Value(${str(v)})"
      case Bind(name, body) => s"Bind(${str(name)}, ${str(body)})"
      case Unapply(fun, implicits, patterns) => s"Unapply(${str(fun)}, ${implicits.map(str)}, ${patterns.map(str)})"
      case Alternative(patterns) => s"Alternative(${patterns.map(str)})"
      case TypeTest(tpt) => s"TypeTest(${str(tpt)})"
      case Wildcard() => s"Wildcard()"
      case _ => "###"
    }
  }

  def str(arg: TermName): String = {
    import scala.tasty._
    arg match {
      case Simple(name) => name.toString // TODO
      case _ => "###"
    }
  }

  def str(arg: PossiblySignedName): String = arg match {
    case arg: TermName => str(arg)
    case _ => "###"
  }

  def str(arg: scala.tasty.typetree.TypeTree): String = {
    import scala.tasty.typetree._
    arg match {
      case Synthetic() => "Synthetic()"
      case Ident(name) => s"Ident(${str(name)})"
      case Select(qual, name) => s"Select(${str(qual)}, ${str(name)})"
      case Singleton(ref) => s"Singleton(${str(ref)})"
      //      case Refined(tpt, refinements) => s"Refined($ref, ${list(refinements)})"
      case Applied(tycon, args) => s"Applied(${str(tycon)}, ${args.map(str)})"
      case TypeBounds(lo, hi) => s"TypeBounds(${str(lo)}, ${str(hi)})"
      case Annotated(arg, annot) => s"Annotated(${str(arg)}, ${str(annot)})"
      case And(left, right) => s"And(${str(left)}, ${str(right)})"
      case Or(left, right) => s"Or(${str(left)}, ${str(right)})"
      case ByName(tpt) => s"ByName(${str(tpt)})"
      case _ => "###"
    }
  }

  def str(arg: scala.tasty.TypeName): String = arg match {
    case _ => arg.toString // TODO
    case _ => "###"
  }

  def str(arg: scala.tasty.Id): String = s"Id(${arg.name})"

  def str(arg: Modifier): String = arg match {
    case _ => ???
  }

  def str(arg: Import.ImportSelector): String = arg match {
    case Import.ImportSelector.Simple(id) => s"Simple(${str(id)})"
    case Import.ImportSelector.Rename(id1, id2) => s"Rename(${str(id1)}, ${str(id2)})"
    case Import.ImportSelector.Omit(id) => s"Omit(${str(id)})"
    case _ => "###"
  }

  def str(arg: scala.tasty.Constant): String = {
    import scala.tasty._
    arg match {
      case Unit() => "Unit()"
      case Null() => "Null()"
      case Boolean(value) => s"Boolean($value)"
      case Byte(value) => s"Byte($value)"
      case Short(value) => s"Short($value)"
      case Char(value) => s"Char('$value')"
      case Int(value) => s"Int($value)"
      case Long(value) => s"Long($value)"
      case Float(value) => s"Float($value)"
      case Double(value) => s"Double($value)"
      case String(value) => s"""String("$value")"""
      case _ => "###"
    }
  }
}
