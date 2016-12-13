package dotty.tools
package dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import core._
import Contexts.Context, Types._, Decorators._, Symbols._, DenotTransformers._
import Denotations._, SymDenotations._, Scopes._, StdNames._

class SpecializeFunction1 extends MiniPhaseTransform with DenotTransformer {
  import ast.tpd._

  val phaseName = "specializeFunction1"

  // TODO: remove me, just used for debug during dev
  def debug[A](str: A*) = {
    str.foreach(println)
    System.exit(1)
  }

  /** Transforms all classes extending `Function1[-T1, +R]` so that
    * they instead extend the specialized version `JFunction...`
    */
  def transform(ref: SingleDenotation)(implicit ctx: Context) = {
    def containsFunction1(xs: List[TypeRef]): Boolean =
      xs.map(_.typeSymbol.showFullName).contains("scala.Function1")

    // TODO: go away **please**
    def blacklisted(cref: ClassDenotation): Boolean = {
      "JFunction1" :: "AbstractFunction1" :: "PartialFunction" :: "AbstractPartialFunction" :: Nil
    } contains (cref.name.show)


    ref match {
      case cref: ClassDenotation if containsFunction1(cref.classParents) && !blacklisted(cref) =>
        specializeFunction1(cref)
      case ref => ref
    }
  }

  def specializeFunction1(cref: ClassDenotation)(implicit ctx: Context): SingleDenotation = {
    def JFunction(t1: String, r: String): TypeRef = ctx.requiredClassRef {
      "scala.compat.java8.JFunction1$mc" + s"$r$t1" + "$sp"
    }

    def typeMember(name: String): ClassSymbol =
      cref.classInfo.decls.lookup(name.toTypeName).info.typeSymbol.asClass

    def t1Member = typeMember("scala$Function1$$T1")
    def rMember  = typeMember("scala$Function1$$R")

    // This Symbol -> String map contains the equaivalent types for
    // scalac-specialized classes
    val typeAbbr = Map(
      defn.DoubleClass  -> "D",
      defn.FloatClass   -> "F",
      defn.IntClass     -> "I",
      defn.LongClass    -> "J",
      defn.UnitClass    -> "V",
      defn.BooleanClass -> "Z"
    )

    def replaceFunction1(in: List[TypeRef]): List[TypeRef] =
      in.foldRight(List.empty[TypeRef]) { (tp, acc) =>
        val curr =
          // TODO: remove .showFullName
          if (tp.typeSymbol.showFullName == "scala.Function1") {
            // If there exists a specialization, both type members will be in
            // `typeAbbr`
            typeAbbr.get(t1Member) -> typeAbbr.get(rMember) match {
              case (Some(t1), Some(r)) => JFunction(t1, r)
              case _ => tp
            }
          }
          else tp

        curr :: acc
      }

    def specializeApply(scope: Scope): Scope =  {
      import core.StdNames._
      def specializeApply(sym: Symbol, t1: String, r: String): Symbol = {
        val specializedMethodName = ("apply$mc" + t1 + r + "$sp").toTermName
        val specializedApply = JFunction(t1, r).info.decls.lookup(specializedMethodName)

        val specSym = ctx.newSymbol(
          cref.symbol.owner,
          specializedMethodName,
          Flags.Override | Flags.Method,
          specializedApply.info
        )

        specSym.enteredAfter(this)
      }

      typeAbbr.get(t1Member) -> typeAbbr.get(rMember) match {
        case (Some(t1), Some(r)) =>
          scope.foldRight(newScope) { (sym, newScope) =>
            newScope.enter {
              if (sym.name eq nme.apply) specializeApply(sym, t1, r) else sym
            }
            newScope
          }
        case _ => scope
      }
    }

    val ClassInfo(prefix, cls, parents, decls, info) = cref.info
    val res = cref.copySymDenotation(
      info = ClassInfo(prefix, cls, replaceFunction1(in = parents), specializeApply(decls), info)
    )

    res
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) = {
    // types as abbreviated in scalac specialized class file names
    val typeAbbr = Map(
      defn.DoubleClass  -> "D",
      defn.FloatClass   -> "F",
      defn.IntClass     -> "I",
      defn.LongClass    -> "J",
      defn.UnitClass    -> "V",
      defn.BooleanClass -> "Z"
    )

    def typeMember(name: String): ClassSymbol =
      ctx.owner.info.asInstanceOf[ClassInfo].decls.lookup(name.toTypeName).info.typeSymbol.asClass

    def t1Member = typeMember("scala$Function1$$T1")
    def rMember  = typeMember("scala$Function1$$R")

    val parentAndTypes =
      tree.parents
      // map super-classes to a tuple of type and tree
      .map { t => (t.tpe, t) }
      // collect the parent that corresponds to Function1 and its type params
      .collect {
        case (RefinedType(RefinedType(parent, _, t1), _, r), function1)
          if parent.typeSymbol.showFullName == "scala.Function1" =>
            (function1, t1, r)
      }
      .headOption

    def replaceParent(parent: Tree, in: List[Tree], withTree: Tree) =
      in.foldRight(List.empty[Tree]) { (t, trees) =>
        (if (parent eq t) withTree else t) :: trees
      }

    def specializedParent(t1: Type, r: Type, orig: Tree): Option[Tree] =
      // get the required class via the abbreviations in `typeAbbr`, if they
      // don't both exist there, there is no specialization for the combination
      typeAbbr.get(t1Member) -> typeAbbr.get(rMember) match {
        case (Some(t1Abbr), Some(rAbbr)) =>
          Some(TypeTree(ctx.requiredClassRef("scala.compat.java8.JFunction1$mc" + rAbbr + t1Abbr +  "$sp")))
        case _ => None
      }

    def withRenamedApply(parent: Tree): List[Tree] = {
      (typeAbbr.get(t1Member), typeAbbr.get(rMember)) match {
        case (Some(t1Abbr), Some(rAbbr)) =>
          tree.body.foldRight(List.empty[Tree]) {
            case (tree: DefDef, acc) if tree.name == nme.apply =>
              val specializedMethodName = ("apply$mc" + t1Abbr + rAbbr + "$sp").toTermName
              val specializedApply = ctx.owner.info.decls.lookup(specializedMethodName).asTerm

              val newDefDef = polyDefDef(specializedApply, trefs => vrefss => {
                tree.rhs
                  .changeOwner(tree.symbol, specializedApply)
                  .subst(tree.vparamss.flatten.map(_.symbol), vrefss.flatten.map(_.symbol))
              })

              newDefDef :: acc
            case (tree, acc) =>
              tree :: acc
          }
        case x => tree.body
      }
    }

    parentAndTypes.flatMap { case (parent, t1, r) =>
      specializedParent(t1, r, parent).map { newPar =>
        val parents = replaceParent(parent, in = tree.parents, withTree = newPar)
        val body = withRenamedApply(newPar)
        cpy.Template(tree)(parents = parents, body = body)
      }
    } getOrElse (tree)
  }
}
