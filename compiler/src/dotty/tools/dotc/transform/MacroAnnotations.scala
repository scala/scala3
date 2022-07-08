package dotty.tools.dotc
package transform

import scala.language.unsafeNulls

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.{InvocationTargetException, Method => JLRMethod}

import core._
import Decorators._
import Flags._
import Types._
import Contexts._
import Symbols._
import Constants._
import ast.Trees._
import ast.{TreeTypeMap, untpd}
import util.Spans._
import SymUtils._
import NameKinds._
import dotty.tools.dotc.ast.tpd
import typer.Implicits.SearchFailureType
import SymDenotations.NoDenotation

import scala.collection.mutable
import dotty.tools.dotc.core.Annotations._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.quoted._
import dotty.tools.dotc.transform.TreeMapWithStages._
import dotty.tools.dotc.typer.ImportInfo.withRootImports
import dotty.tools.dotc.ast.TreeMapWithImplicits

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameKinds.FlatName
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Denotations.staticRef
import dotty.tools.dotc.core.{NameKinds, TypeErasure}
import dotty.tools.dotc.core.Constants.Constant

import dotty.tools.dotc.util.SrcPos

import scala.annotation.constructorOnly
import scala.annotation.tailrec
import scala.annotation.MacroAnnotation
import scala.collection.mutable.ListBuffer
import scala.quoted._
import scala.quoted.runtime.impl.{QuotesImpl, SpliceScope}
import java.lang.reflect.Method


object ObjectTrees:
  import tpd._
  def unapply(tree: Tree)(using Context): Option[(Tree, Tree)] = tree match
    case Thicket(List(valT, clsT)) if valT.symbol.is(ModuleVal) && clsT.symbol.is(ModuleClass) &&
                                      valT.symbol.moduleClass == clsT.symbol =>
      Some((valT, clsT))
    case _ => None

object MacroAnnotationTransformer {
  import tpd._

  def instantiateAnnot(tree: Tree, annot: Annotation)(using Context): MacroAnnotation =
    val interpreter = new InterpreterMacroAnnot(tree.srcPos, MacroClassLoader.fromContext)
    interpreter.interpret[Object](annot.tree) match {
      case Some(obj) => obj.asInstanceOf[MacroAnnotation]
      case None =>
        throw new Exception(s"Macro annotation ${annot.tree.symbol} cannot be instantiated")
    }

  @tailrec
  def transform(trees: List[Tree], pending: Set[Symbol])(using Context): List[Tree] = pending match
    case _ if pending.size == 0 =>
      assert(!trees.isEmpty)
      trees
    case _ =>
      def getSymbol(tree: Tree)(using Context): List[Symbol] = tree match
        case ObjectTrees(valT, clsT) => List(valT.symbol, clsT.symbol)
        case _ => List(tree.symbol)

      val expandedTrees = trees.flatMap {
        case ObjectTrees(valT, clsT) => transformDef(valT, Some(clsT), pending)
        case tree => transformDef(tree, None, pending)
      }
      transform(expandedTrees, pending union expandedTrees.flatMap(getSymbol(_)).toSet diff trees.flatMap(getSymbol(_)).toSet)

  def transformDef(tree: Tree, modCls: Option[Tree], pending: Set[Symbol])(using Context): List[Tree] =

    def isMacroAnnotation(annot: Annotation)(using Context): Boolean =
      val sym = annot.tree.symbol
      sym.denot != NoDenotation && sym.owner.derivesFrom(defn.QuotedMacroAnnotationClass)

    def enterNewDefInClass(tree: Tree)(using Context): Tree =
      tree match
        case tree @ TypeDef(_, tmpl: Template) =>
          tmpl.body.foreach{case t: DefTree =>
            if !t.symbol.owner.info.decls.contains(t.symbol.name, t.symbol) then
              t.symbol.entered
          }
        case _ => ()
      tree

    if !pending.contains(tree.symbol) then
      return List(tree)
    if level != 0 then
      return List(tree)

    val annotTree = tree.symbol.annotations.filter(isMacroAnnotation(_))
    val annotParam = tree match
      case tree @ DefDef(_, paramss, _, _) =>
        paramss.flatMap{ params => params.flatMap{ param =>
          param.symbol.annotations.filter(isMacroAnnotation(_)).map((_, param))}}
      case _ => Nil

    val annots: List[Annotation | (Annotation, DefTree)] = annotTree ++ annotParam
    if annots.isEmpty then
      modCls match
        case Some(clsT) => List(tree, clsT)
        case None => List(tree)

    var modClsTmp: Option[Tree] = modCls
    var newTreesBefore = new ListBuffer[Tree]() // ListBuilder
    var newTreesAfter = new ListBuffer[Tree]()

    val transformedTree: Tree = annots.foldLeft(tree){ (tree, annot) =>

      given quotes: Quotes = QuotesImpl()(using SpliceScope.contextWithNewSpliceScope(tree.symbol.sourcePos)(using MacroExpansion.context(tree)).withOwner(tree.symbol))
      given Conversion[Tree, quotes.reflect.Definition] = _.asInstanceOf[quotes.reflect.Definition]
      val transformedTrees = annot match
        case (annot, param) =>
          instantiateAnnot(param, annot).asInstanceOf[MacroAnnotation].
            transformParam(param, tree).asInstanceOf[List[Tree]]
        case annot: Annotation => modClsTmp match
          case Some(modCls) =>
            val convertedModVal = tree.asInstanceOf[quotes.reflect.ValDef]
            val convertedModCls = modCls.asInstanceOf[quotes.reflect.TypeDef]
            instantiateAnnot(tree, annot).asInstanceOf[MacroAnnotation].
              transformObject(convertedModVal, convertedModCls).asInstanceOf[List[Tree]]
          case None =>
            instantiateAnnot(tree, annot).asInstanceOf[MacroAnnotation].
              transform(tree).asInstanceOf[List[Tree]]

      val (before, selfAndAfter) = transformedTrees.splitAt(transformedTrees.map(_.symbol).indexOf(tree.symbol))
      val after = modClsTmp match
        case Some(_) =>
          modClsTmp = Some(selfAndAfter.tail.head)
          enterNewDefInClass(modClsTmp.get)
          selfAndAfter.tail.tail
        case None => selfAndAfter.tail
      (before ++ after).foreach{case t: DefTree =>
        // should not have this check, will be removed afterwards
        if !t.symbol.owner.info.decls.contains(t.symbol.name, t.symbol) then
          t.symbol.entered
      }
      newTreesBefore.appendAll(before)
      newTreesAfter.prependAll(after)
      enterNewDefInClass(selfAndAfter.head)
    }
    newTreesBefore.toList ++ List(transformedTree) ++ modCls.toList ++ newTreesAfter.toList
}

class InterpreterMacroAnnot(pos: SrcPos, classLoader: ClassLoader)(using Context) extends Interpreter(pos, classLoader):
  import tpd._
  override def interpretTree(tree: Tree)(implicit env: Env): Object = tree match {
    case Apply(Select(New(annot), _), args) =>
      val interpretedArgs = args.map(interpret[Object](_).get)
      interpretNew(tree.symbol, interpretedArgs)

    case _ => super.interpretTree(tree)
  }

  override def interpretNew(fn: Symbol, args: => List[Object])(implicit env: Env): Object = {
    val clazz = loadClass(fn.owner.fullName.toString.replaceAll("\\$\\.", "\\$"))
    val constr = clazz.getConstructor(paramsSig(fn): _*)
    constr.newInstance(args: _*).asInstanceOf[Object]
  }

  def getMethod(clazz: Class[_], methodSym: Symbol): Method =
    super.getMethod(clazz, methodSym.name.asTermName, paramsSig(methodSym))
