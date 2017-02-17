package dotty.tools.dotc
package macros

import core._
import Contexts.Context
import Decorators._
import ast._
import Trees._
import util._
import Names._
import StdNames._
import Flags._
import typer.ErrorReporting._
import Constants._
import config.Printers.{ macros => debug }

/** Transform macros definitions
 *
 *  Macro definition is transformed from:
 *
 *    class macros {
 *      inline def f[T](a: A)(b: B): C = meta {
 *        body
 *      }
 *    }
 *
 *  to:
 *
 *    class main {
 *      <macro> def f[T](a: A)(b: B): C = null
 *    }
 *
 *    object main$inline {
 *      def f(prefix: scala.meta.Term)(T: scala.meta.Type)(a: scala.meta.Term)(b: scala.meta.Term): scala.meta.Tree = body
 *    }
 */

private[macros] object Transform {
  import untpd._

  val INLINE_SUFFIX = nme.NAME_JOIN + "inline"

  /** Transform macros definitions inside class definitions (see the note above)
   */
  def transform(tree: TypeDef)(implicit ctx: Context): Tree = {
    if (!tree.isClassDef) return tree

    val tmpl = tree.rhs.asInstanceOf[Template]

    val macros = getMacros(tmpl)
    if (macros.isEmpty) return tree

    val isAnnotMacroDef = isAnnotMacro(tmpl)
    val moduleName = tree.name + INLINE_SUFFIX
    val implObj = createImplObject(moduleName, macros, isAnnotMacroDef)

    val treeNew = cpy.TypeDef(tree)(rhs = newTemplate(tmpl, macros))

    debug.println(i"macro definition: $tree transformed to { $treeNew; \n ${implObj} }")

    Thicket(List(treeNew, implObj))
  }

  /** Transform macros definitions inside object definitions (see the note above)
   */
  def transform(tree: ModuleDef)(implicit ctx: Context): Tree = {
    val macros = getMacros(tree.impl)
    if (macros.isEmpty) return tree

    val moduleName = tree.name + nme.MODULE_SUFFIX.toString + INLINE_SUFFIX
    val implObj = createImplObject(moduleName, macros, false)

    val treeNew = cpy.ModuleDef(tree)(impl = newTemplate(tree.impl, macros), name = tree.name)

    debug.println(i"macro definition: $tree transformed to { $treeNew; \n ${implObj} }")

    Thicket(List(treeNew, implObj))
  }

  /** Does the template extend `StaticAnnotation`?
   */
  def isAnnotMacro(tmpl: Template)(implicit ctx: Context): Boolean = {
    val acc = new UntypedTreeAccumulator[Boolean] {
      def apply(cur: Boolean, tree: Tree)(implicit ctx: Context): Boolean = cur || (tree match {
        case Ident(name)     => name.toString == "StaticAnnotation"
        case Select(_, name) => name.toString == "StaticAnnotation"
        case _ => foldOver(cur, tree)
      })

      override def foldOver(cur: Boolean, tree: Tree)(implicit ctx: Context): Boolean =
        if (cur) cur else super.foldOver(cur, tree)
    }

    tmpl.parents.exists(acc(false, _))
  }

  def newTemplate(tmpl: Template, macros: List[DefDef])(implicit ctx: Context): Template = {
     // modify macros body and flags
    val macrosNew = macros.map { m =>
      val mdef = cpy.DefDef(m)(rhs = Ident("???".toTermName))
      mdef.withMods((mdef.mods | Macro) &~ Inline )
    }

    val bodyNew = macrosNew ++ tmpl.body.diff(macros)
    cpy.Template(tmpl)(body = bodyNew)
  }

  def getMacros(tmpl: Template)(implicit ctx: Context): List[DefDef] = tmpl.body.filter {
    case mdef : DefDef =>
      val rhsValid = mdef.rhs match {
        case Apply(Ident(nme.meta), _) => true
        case _ => false
      }

      mdef.mods.is(Inline) && rhsValid
    case _ => false
  }.asInstanceOf[List[DefDef]]


  /** Create macro implementation method
   *
   *     inline def f[T](a: A)(b: B): C = meta { body }
   *
   *  will be implemented by
   *
   *    def f(prefix: scala.meta.Term)
   *         (T: scala.meta.Type)
   *         (a: scala.meta.Term)(b: scala.meta.Term): scala.meta.Tree = body
   *
   *  with `this` replaced by `prefix` in `body`
   *
   */
  private def createImplMethod(defn: DefDef, isAnnotMacroDef: Boolean)(implicit ctx: Context): DefDef = {
    val Apply(_, rhs :: Nil) = defn.rhs

    val scala = Ident("scala".toTermName)
    val scalaTerm = Select(scala, "meta".toTermName)
    val treeType = Select(scalaTerm, "Tree".toTypeName)
    val termType = Select(scalaTerm, "Term".toTypeName)
    val typeType = Select(scalaTerm, "Type".toTypeName)

    val prefix = ValDef("prefix".toTermName, termType, EmptyTree).withFlags(TermParam)
    val typeParams = for (tdef: TypeDef <- defn.tparams)
      yield ValDef(tdef.name.toTermName, typeType, EmptyTree).withFlags(TermParam)

    val termParams = for (params <- defn.vparamss)
      yield params.map { case vdef: ValDef =>
        val tp = if (isAnnotMacroDef) treeType else termType
        ValDef(vdef.name.toTermName, tp, EmptyTree).withFlags(TermParam)
      }

    val params =
      if (typeParams.size > 0) List(prefix) :: typeParams :: termParams
      else List(prefix) :: termParams

    // replace `this` with `prefix`
    val mapper = new UntypedTreeMap {
      override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case tree: This =>
          Ident("prefix".toTermName).withPos(tree.pos)
        case _ =>
          super.transform(tree)
      }
    }

    val body = mapper.transform(rhs)
    DefDef(defn.name, Nil, params, treeType, body).withFlags(Synthetic)
  }

  /** create object A$inline to hold all macros implementations for class A */
  def createImplObject(name: String, macros: List[DefDef], isAnnotMacroDef: Boolean)(implicit ctx: Context): Tree = {
    val methods = macros.map(createImplMethod(_, isAnnotMacroDef))
    val constr = DefDef(nme.CONSTRUCTOR, Nil, Nil, TypeTree(), EmptyTree)
    val templ = Template(constr, Nil, EmptyValDef, methods)
    ModuleDef(name.toTermName, templ)
  }
}
