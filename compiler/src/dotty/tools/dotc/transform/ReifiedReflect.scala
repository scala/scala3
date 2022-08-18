package dotty.tools.dotc
package transform

import core._
import Decorators._
import Flags._
import Types._
import Contexts._
import Symbols._
import SymUtils._
import NameKinds._
import dotty.tools.dotc.ast.tpd
import tpd._

import scala.collection.mutable
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.transform.TreeMapWithStages._

import scala.annotation.constructorOnly

/** Helper methods to construct trees calling methods in `Quotes.reflect` based on the current `quotes` tree */
trait ReifiedReflect:

  /** Stable reference to the instance of `scala.quoted.Quotes` */
  def quotesTree: Tree

  def self(using Context): Tree =
    quotesTree.select(defn.Quotes_reflect)

  /** Create type for `quotes.reflect.Term` */
  def TermTpt(using Context) =
    self.select(defn.Quotes_reflect_TermType)

  /** Create type for `quotes.reflect.TypeTree` */
  def TypeTreeTpt(using Context) =
    self.select(defn.Quotes_reflect_TypeTreeType)

  /** Create tree for `quotes.reflect.Apply(<fn>, List(<args>*))` */
  def Apply(fn: Tree, args: List[Tree])(using Context) =
    val argTrees = tpd.mkList(args, TermTpt)
    self.select(defn.Quotes_reflect_Apply)
      .select(defn.Quotes_reflect_Apply_apply)
      .appliedTo(fn, argTrees)

  /** Create tree for `quotes.reflect.TypeApply(<fn>, List(<args>*))` */
  def TypeApply(fn: Tree, args: List[Tree])(using Context) =
    val argTrees = tpd.mkList(args, TypeTreeTpt)
    self.select(defn.Quotes_reflect_TypeApply)
      .select(defn.Quotes_reflect_TypeApply_apply)
      .appliedTo(fn, argTrees)

  /** Create tree for `quotes.reflect.Assing(<lhs>, <rhs>)` */
  def Assign(lhs: Tree, rhs: Tree)(using Context) =
    self.select(defn.Quotes_reflect_Assign)
      .select(defn.Quotes_reflect_Assign_apply)
      .appliedTo(lhs, rhs)

  /** Create tree for `quotes.reflect.Inferred(<typeTree>)` */
  def Inferred(typeTree: Tree)(using Context) =
    self.select(defn.Quotes_reflect_Inferred)
      .select(defn.Quotes_reflect_Inferred_apply)
      .appliedTo(typeTree)

  /** Create tree for `quotes.reflect.Literal(<constant>)` */
  def Literal(constant: Tree)(using Context) =
    self.select(defn.Quotes_reflect_Literal)
      .select(defn.Quotes_reflect_Literal_apply)
      .appliedTo(constant)

  /** Create tree for `quotes.reflect.TypeRepr.of(Type.of[<tpe>](quotes))` */
  def TypeReprOf(tpe: Type)(using Context) =
    self.select(defn.Quotes_reflect_TypeRepr)
      .select(defn.Quotes_reflect_TypeRepr_of)
      .appliedToType(tpe)
      .appliedTo(
        ref(defn.QuotedTypeModule_of)
          .appliedToType(tpe)
          .appliedTo(quotesTree)
      )

  /** Create tree for `quotes.reflect.TypeRepr.typeConstructorOf(<classTree>)` */
  def TypeRepr_typeConstructorOf(classTree: Tree)(using Context) =
    self.select(defn.Quotes_reflect_TypeRepr)
      .select(defn.Quotes_reflect_TypeRepr_typeConstructorOf)
      .appliedTo(classTree)

  /** Create tree for `quotes.reflect.asTerm(<expr>)` */
  def asTerm(expr: Tree)(using Context) =
    self.select(defn.Quotes_reflect_asTerm)
      .appliedTo(expr)

  /** Create tree for `quotes.reflect.TypeReprMethods.asType(<typeRepr>)` */
  def asType(tpe: Type)(typeRepr: Tree)(using Context) =
    self.select(defn.Quotes_reflect_TypeReprMethods)
      .select(defn.Quotes_reflect_TypeReprMethods_asType)
      .appliedTo(typeRepr)
      .asInstance(defn.QuotedTypeClass.typeRef.appliedTo(tpe))

  /** Create tree for `quotes.reflect.TreeMethods.asExpr(<term>).asInstanceOf[<tpe>]` */
  def asExpr(tpe: Type)(term: Tree)(using Context) =
    self.select(defn.Quotes_reflect_TreeMethods)
      .select(defn.Quotes_reflect_TreeMethods_asExpr)
      .appliedTo(term)
      .asInstance(defn.QuotedExprClass.typeRef.appliedTo(tpe))

end ReifiedReflect
