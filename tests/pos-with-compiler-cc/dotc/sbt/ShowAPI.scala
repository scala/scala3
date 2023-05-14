// This file is copied straight from
// https://github.com/sbt/sbt/blob/0.13/compile/api/src/main/scala/xsbt/api/ShowAPI.scala
// It is convenient to be able to pretty-print the API from Dotty itself to test
// the sbt phase without having to run sbt.

/* sbt -- Simple Build Tool
 * Copyright 2010 Mark Harrah
 */
package dotty.tools.dotc
package sbt

import scala.language.unsafeNulls

import xsbti.api._

import scala.util.Try

object DefaultShowAPI {
  private lazy val defaultNesting = Try { java.lang.Integer.parseInt(sys.props.get("sbt.inc.apidiff.depth").get) } getOrElse 2

  def apply(d: Definition): String = ShowAPI.showDefinition(d)(defaultNesting)
  def apply(d: Type): String = ShowAPI.showType(d)(defaultNesting)
  def apply(a: ClassLike): String = ShowAPI.showApi(a)(defaultNesting)
}

object ShowAPI {
  private lazy val numDecls = Try { java.lang.Integer.parseInt(sys.props.get("sbt.inc.apidiff.decls").get) } getOrElse 0

  private def truncateDecls(decls: Array[ClassDefinition]): Array[ClassDefinition] = if (numDecls <= 0) decls else decls.take(numDecls)
  private def lines(ls: Seq[String]): String = ls.mkString("\n", "\n", "\n")

  def showApi(c: ClassLike)(implicit nesting: Int): String =
    showDefinition(c)

  def showDefinition(d: Definition)(implicit nesting: Int): String = d match {
    case v: Val              => showMonoDef(v, "val") + ": " + showType(v.tpe)
    case v: Var              => showMonoDef(v, "var") + ": " + showType(v.tpe)
    case d: Def              => showPolyDef(d, "def") + showValueParams(d.valueParameters.toIndexedSeq) + ": " + showType(d.returnType)
    case ta: TypeAlias       => showPolyDef(ta, "type") + " = " + showType(ta.tpe)
    case td: TypeDeclaration => showPolyDef(td, "type") + showBounds(td.lowerBound, td.upperBound)
    case cl: ClassLike => showMonoDef(d, showDefinitionType(cl.definitionType)) +
      showTypeParameters(cl.typeParameters.toIndexedSeq) + " extends " + showTemplate(cl)
    case cl: ClassLikeDef => showPolyDef(cl, showDefinitionType(cl.definitionType))
  }

  private def showTemplate(cl: ClassLike)(implicit nesting: Int) =
    if (nesting <= 0) "<nesting level reached>"
    else {
      val showSelf = if (cl.selfType.isInstanceOf[EmptyType]) "" else " self: " + showNestedType(cl.selfType) + " =>"

      cl.structure.parents.map(showNestedType).mkString("", " with ", " {") + showSelf +
        lines(truncateDecls(cl.structure.inherited).toIndexedSeq.map(d => "^inherited^ " + showNestedDefinition(d))) +
        lines(truncateDecls(cl.structure.declared).toIndexedSeq.map(showNestedDefinition)) +
        "}"
    }

  def showType(t: Type)(implicit nesting: Int): String = t match {
    case st: Projection   => showType(st.prefix) + "#" + st.id
    case st: ParameterRef => "<" + st.id + ">"
    case st: Singleton    => showPath(st.path)
    case st: EmptyType    => "<empty>"
    case p: Parameterized => showType(p.baseType) + p.typeArguments.map(showType).mkString("[", ", ", "]")
    case c: Constant      => showType(c.baseType) + "(" + c.value + ")"
    case a: Annotated     => showAnnotations(a.annotations.toIndexedSeq) + " " + showType(a.baseType)
    case s: Structure =>
      s.parents.map(showType).mkString(" with ") + (
        if (nesting <= 0) "{ <nesting level reached> }"
        else truncateDecls(s.declared).map(showNestedDefinition).mkString(" {", "\n", "}")
      )
    case e: Existential =>
      showType(e.baseType) + (
        if (nesting <= 0) " forSome { <nesting level reached> }"
        else e.clause.map(t => "type " + showNestedTypeParameter(t)).mkString(" forSome { ", "; ", " }")
      )
    case p: Polymorphic => showType(p.baseType) + (
      if (nesting <= 0) " [ <nesting level reached> ]"
      else showNestedTypeParameters(p.parameters.toIndexedSeq)
    )
  }

  private def showPath(p: Path): String = p.components.map(showPathComponent).mkString(".")
  private def showPathComponent(pc: PathComponent) = pc match {
    case s: Super => "super[" + showPath(s.qualifier) + "]"
    case _: This  => "this"
    case i: Id    => i.id
  }

  private def space(s: String) = if (s.isEmpty) s else s + " "
  private def showMonoDef(d: Definition, label: String)(implicit nesting: Int): String =
    space(showAnnotations(d.annotations.toIndexedSeq)) + space(showAccess(d.access)) + space(showModifiers(d.modifiers)) + space(label) + d.name

  private def showPolyDef(d: ParameterizedDefinition, label: String)(implicit nesting: Int): String =
    showMonoDef(d, label) + showTypeParameters(d.typeParameters.toIndexedSeq)

  private def showTypeParameters(tps: Seq[TypeParameter])(implicit nesting: Int): String =
    if (tps.isEmpty) ""
    else tps.map(showTypeParameter).mkString("[", ", ", "]")

  private def showTypeParameter(tp: TypeParameter)(implicit nesting: Int): String =
    showAnnotations(tp.annotations.toIndexedSeq) + " " + showVariance(tp.variance) + tp.id + showTypeParameters(tp.typeParameters.toIndexedSeq) + " " + showBounds(tp.lowerBound, tp.upperBound)

  private def showAnnotations(as: Seq[Annotation])(implicit nesting: Int) = as.map(showAnnotation).mkString(" ")
  private def showAnnotation(a: Annotation)(implicit nesting: Int) =
    "@" + showType(a.base) + (
      if (a.arguments.isEmpty) ""
      else a.arguments.map(a => a.name + " = " + a.value).mkString("(", ", ", ")")
    )

  private def showBounds(lower: Type, upper: Type)(implicit nesting: Int): String = ">: " + showType(lower) + " <: " + showType(upper)

  private def showValueParams(ps: Seq[ParameterList])(implicit nesting: Int): String =
    ps.map(pl =>
      pl.parameters.map(mp =>
        mp.name + ": " + showParameterModifier(showType(mp.tpe), mp.modifier) + (if (mp.hasDefault) "= ..." else "")).mkString(if (pl.isImplicit) "(implicit " else "(", ", ", ")")).mkString("")

  private def showParameterModifier(base: String, pm: ParameterModifier): String = pm match {
    case ParameterModifier.Plain    => base
    case ParameterModifier.Repeated => base + "*"
    case ParameterModifier.ByName   => "=> " + base
  }

  private def showDefinitionType(d: DefinitionType) = d match {
    case DefinitionType.Trait         => "trait"
    case DefinitionType.ClassDef      => "class"
    case DefinitionType.Module        => "object"
    case DefinitionType.PackageModule => "package object"
  }

  private def showAccess(a: Access) = a match {
    case p: Public    => ""
    case p: Protected => "protected" + showQualifier(p.qualifier)
    case p: Private   => "private" + showQualifier(p.qualifier)
  }

  private def showQualifier(q: Qualifier) = q match {
    case _: Unqualified   => ""
    case _: ThisQualifier => "[this]"
    case i: IdQualifier   => "[" + i.value + "]"
  }

  private def showModifiers(m: Modifiers) = List(
    (m.isOverride, "override"),
    (m.isFinal, "final"),
    (m.isSealed, "sealed"),
    (m.isImplicit, "implicit"),
    (m.isAbstract, "abstract"),
    (m.isLazy, "lazy")
  ).collect { case (true, mod) => mod }.mkString(" ")

  private def showVariance(v: Variance) = v match {
    case Variance.Invariant     => ""
    case Variance.Covariant     => "+"
    case Variance.Contravariant => "-"
  }

  // limit nesting to prevent cycles and generally keep output from getting humongous
  private def showNestedType(tp: Type)(implicit nesting: Int) = showType(tp)(nesting - 1)
  private def showNestedTypeParameter(tp: TypeParameter)(implicit nesting: Int) = showTypeParameter(tp)(nesting - 1)
  private def showNestedTypeParameters(tps: Seq[TypeParameter])(implicit nesting: Int) = showTypeParameters(tps)(nesting - 1)
  private def showNestedDefinition(d: Definition)(implicit nesting: Int) = showDefinition(d)(nesting - 1)
}

