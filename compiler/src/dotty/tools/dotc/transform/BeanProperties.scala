package dotty.tools.dotc
package transform

import core.*
import ast.tpd.*
import Annotations.*
import Contexts.*
import Symbols.*
import SymUtils.*
import Decorators.*
import Flags.*
import Names.*
import Types.*
import util.Spans.*

import DenotTransformers.*

class BeanProperties(thisPhase: DenotTransformer):
  def addBeanMethods(impl: Template)(using Context): Template =
    val origBody = impl.body
    cpy.Template(impl)(body = impl.body.flatMap {
      case v: ValDef => generateAccessors(v)
      case _ => Nil
    } ::: origBody)

  def generateAccessors(valDef: ValDef)(using Context): List[Tree] =
    def generateGetter(valDef: ValDef, annot: Annotation)(using Context) : Tree =
      val prefix = if annot matches defn.BooleanBeanPropertyAnnot then "is" else "get"
      val meth = newSymbol(
        owner = ctx.owner,
        name = prefixedName(prefix, valDef.name),
        flags = Method | Synthetic | Invisible,
        info = MethodType(Nil, valDef.denot.info),
        coord = annot.tree.span
      ).enteredAfter(thisPhase).asTerm
       .withAnnotationsCarrying(valDef.symbol, defn.BeanGetterMetaAnnot)
      val body: Tree = ref(valDef.symbol)
      DefDef(meth, body).withSpan(meth.span)

    def maybeGenerateSetter(valDef: ValDef, annot: Annotation)(using Context): Option[Tree] =
      Option.when(valDef.denot.asSymDenotation.flags.is(Mutable)) {
        val owner = ctx.owner
        val meth = newSymbol(
          owner,
          name = prefixedName("set", valDef.name),
          flags = Method | Synthetic | Invisible,
          info = MethodType(valDef.name :: Nil, valDef.denot.info :: Nil, defn.UnitType),
          coord = annot.tree.span
        ).enteredAfter(thisPhase).asTerm
         .withAnnotationsCarrying(valDef.symbol, defn.BeanSetterMetaAnnot)
        def body(params: List[List[Tree]]): Tree = Assign(ref(valDef.symbol), params.head.head)
        DefDef(meth, body).withSpan(meth.span)
      }

    def prefixedName(prefix: String, valName: Name) =
      (prefix + valName.lastPart.toString.capitalize).toTermName

    val symbol = valDef.denot.symbol
    symbol.getAnnotation(defn.BeanPropertyAnnot)
      .orElse(symbol.getAnnotation(defn.BooleanBeanPropertyAnnot))
      .toList.flatMap { annot =>
        generateGetter(valDef, annot) +: maybeGenerateSetter(valDef, annot) ++: Nil
      }
  end generateAccessors
