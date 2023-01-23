package dotty.tools.dotc
package transform

import core._
import ast.tpd._
import Annotations._
import Contexts._
import Symbols.newSymbol
import Decorators._
import Flags._
import Names._
import Types._
import util.Spans._

import DenotTransformers._

class BeanProperties(thisPhase: DenotTransformer):
  def addBeanMethods(impl: Template)(using Context): Template =
    val origBody = impl.body
    cpy.Template(impl)(body = impl.body.flatMap {
      case v: ValDef => generateAccessors(v)
      case _ => Nil
    } ::: origBody)

  def generateAccessors(valDef: ValDef)(using Context): List[Tree] =
    import Symbols.defn

    def generateGetter(valDef: ValDef, annot: Annotation)(using Context) : Tree =
      val prefix = if annot matches defn.BooleanBeanPropertyAnnot then "is" else "get"
      val meth = newSymbol(
        owner = ctx.owner,
        name = prefixedName(prefix, valDef.name),
        flags = Method | Synthetic | Invisible,
        info = MethodType(Nil, valDef.denot.info),
        coord = annot.tree.span
      ).enteredAfter(thisPhase).asTerm
      meth.addAnnotations(valDef.symbol.annotations)
      val body: Tree = ref(valDef.symbol)
      DefDef(meth, body)

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
        meth.addAnnotations(valDef.symbol.annotations)
        def body(params: List[List[Tree]]): Tree = Assign(ref(valDef.symbol), params.head.head)
        DefDef(meth, body)
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
