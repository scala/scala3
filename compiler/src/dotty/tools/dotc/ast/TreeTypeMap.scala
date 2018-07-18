package dotty.tools
package dotc
package ast

import core._
import Types._, Contexts._, Constants._, Names._, Flags._
import SymDenotations._, Symbols._, Annotations._, Trees._, Symbols._
import Denotations._, Decorators._
import dotty.tools.dotc.transform.SymUtils._
import core.tasty.TreePickler.Hole

/** A map that applies three functions and a substitution together to a tree and
 *  makes sure they are coordinated so that the result is well-typed. The functions are
 *  @param typeMap   A function from Type to Type that gets applied to the
 *                   type of every tree node and to all locally defined symbols,
 *                   followed by the substitution [substFrom := substTo].
 *  @param treeMap   A transformer that translates all encountered subtrees in
 *                   prefix traversal orders
 *  @param oldOwners Previous owners. If a top-level local symbol in the mapped tree
 *                   has one of these as an owner, the owner is replaced by the corresponding
 *                   symbol in `newOwners`.
 *  @param newOwners New owners, replacing previous owners.
 *  @param substFrom The symbols that need to be substituted.
 *  @param substTo   The substitution targets.
 *
 *  The reason the substitution is broken out from the rest of the type map is
 *  that all symbols have to be substituted at the same time. If we do not do this,
 *  we risk data races on named types. Example: Say we have `outer#1.inner#2` and we
 *  have two substitutions S1 = [outer#1 := outer#3], S2 = [inner#2 := inner#4] where
 *  hashtags precede symbol ids. If we do S1 first, we get outer#2.inner#3. If we then
 *  do S2 we get outer#2.inner#4. But that means that the named type outer#2.inner
 *  gets two different denotations in the same period. Hence, if -Yno-double-bindings is
 *  set, we would get a data race assertion error.
 */
class TreeTypeMap(
  val typeMap: Type => Type = IdentityTypeMap,
  val treeMap: tpd.Tree => tpd.Tree = identity _,
  val oldOwners: List[Symbol] = Nil,
  val newOwners: List[Symbol] = Nil,
  val substFrom: List[Symbol] = Nil,
  val substTo: List[Symbol] = Nil)(implicit ctx: Context) extends tpd.TreeMap {
  import tpd._

  /** If `sym` is one of `oldOwners`, replace by corresponding symbol in `newOwners` */
  def mapOwner(sym: Symbol) = sym.subst(oldOwners, newOwners)

  /** Replace occurrences of `This(oldOwner)` in some prefix of a type
   *  by the corresponding `This(newOwner)`.
   */
  private val mapOwnerThis = new TypeMap {
    private def mapPrefix(from: List[Symbol], to: List[Symbol], tp: Type): Type = from match {
      case Nil => tp
      case (cls: ClassSymbol) :: from1 => mapPrefix(from1, to.tail, tp.substThis(cls, to.head.thisType))
      case _ :: from1 => mapPrefix(from1, to.tail, tp)
    }
    def apply(tp: Type): Type = tp match {
      case tp: NamedType => tp.derivedSelect(mapPrefix(oldOwners, newOwners, tp.prefix))
      case _ => mapOver(tp)
    }
  }

  def mapType(tp: Type) =
    mapOwnerThis(typeMap(tp).substSym(substFrom, substTo))

  private def updateDecls(prevStats: List[Tree], newStats: List[Tree]): Unit =
    if (prevStats.isEmpty) assert(newStats.isEmpty)
    else {
      prevStats.head match {
        case pdef: MemberDef =>
          val prevSym = pdef.symbol
          val newSym = newStats.head.symbol
          val newCls = newSym.owner.asClass
          if (prevSym != newSym) newCls.replace(prevSym, newSym)
        case _ =>
      }
      updateDecls(prevStats.tail, newStats.tail)
    }

  override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = treeMap(tree) match {
    case impl @ Template(constr, parents, self, _) =>
      val tmap = withMappedSyms(localSyms(impl :: self :: Nil))
      cpy.Template(impl)(
          constr = tmap.transformSub(constr),
          parents = parents.mapconserve(transform),
          self = tmap.transformSub(self),
          body = impl.body mapconserve
            (tmap.transform(_)(ctx.withOwner(mapOwner(impl.symbol.owner))))
        ).withType(tmap.mapType(impl.tpe))
    case tree1 =>
      tree1.withType(mapType(tree1.tpe)) match {
        case id: Ident if tpd.needsSelect(id.tpe) =>
          ref(id.tpe.asInstanceOf[TermRef]).withPos(id.pos)
        case ddef @ DefDef(name, tparams, vparamss, tpt, _) =>
          val (tmap1, tparams1) = transformDefs(ddef.tparams)
          val (tmap2, vparamss1) = tmap1.transformVParamss(vparamss)
          val res = cpy.DefDef(ddef)(name, tparams1, vparamss1, tmap2.transform(tpt), tmap2.transform(ddef.rhs))
          res.symbol.transformAnnotations {
            case ann: BodyAnnotation => ann.derivedAnnotation(res.rhs)
            case ann => ann
          }
          res
        case tdef @ LambdaTypeTree(tparams, body) =>
          val (tmap1, tparams1) = transformDefs(tparams)
          cpy.LambdaTypeTree(tdef)(tparams1, tmap1.transform(body))
        case blk @ Block(stats, expr) =>
          val (tmap1, stats1) = transformDefs(stats)
          val expr1 = tmap1.transform(expr)
          cpy.Block(blk)(stats1, expr1)
        case inlined @ Inlined(call, bindings, expanded) =>
          val (tmap1, bindings1) = transformDefs(bindings)
          val expanded1 = tmap1.transform(expanded)
          cpy.Inlined(inlined)(call, bindings1, expanded1)
        case cdef @ CaseDef(pat, guard, rhs) =>
          val tmap = withMappedSyms(patVars(pat))
          val pat1 = tmap.transform(pat)
          val guard1 = tmap.transform(guard)
          val rhs1 = tmap.transform(rhs)
          cpy.CaseDef(cdef)(pat1, guard1, rhs1)
        case Hole(n, args) =>
          Hole(n, args.mapConserve(transform)).withPos(tree.pos).withType(mapType(tree.tpe))
        case tree1 =>
          super.transform(tree1)
      }
  }

  override def transformStats(trees: List[tpd.Tree])(implicit ctx: Context) =
    transformDefs(trees)._2

  private def transformDefs[TT <: tpd.Tree](trees: List[TT])(implicit ctx: Context): (TreeTypeMap, List[TT]) = {
    val tmap = withMappedSyms(tpd.localSyms(trees))
    (tmap, tmap.transformSub(trees))
  }

  private def transformVParamss(vparamss: List[List[ValDef]]): (TreeTypeMap, List[List[ValDef]]) = vparamss match {
    case vparams :: rest =>
      val (tmap1, vparams1) = transformDefs(vparams)
      val (tmap2, vparamss2) = tmap1.transformVParamss(rest)
      (tmap2, vparams1 :: vparamss2)
    case nil =>
      (this, vparamss)
  }

  def apply[ThisTree <: tpd.Tree](tree: ThisTree): ThisTree = transform(tree).asInstanceOf[ThisTree]

  def apply(annot: Annotation): Annotation = annot.derivedAnnotation(apply(annot.tree))

  /** The current tree map composed with a substitution [from -> to] */
  def withSubstitution(from: List[Symbol], to: List[Symbol]): TreeTypeMap =
    if (from eq to) this
    else {
      // assert that substitution stays idempotent, assuming its parts are
      // TODO: It might be better to cater for the asserted-away conditions, by
      // setting up a proper substitution abstraction with a compose operator that
      // guarantees idempotence. But this might be too inefficient in some cases.
      // We'll cross that bridge when we need to.
      assert(!from.exists(substTo contains _))
      assert(!to.exists(substFrom contains _))
      assert(!from.exists(newOwners contains _))
      assert(!to.exists(oldOwners contains _))
      new TreeTypeMap(
        typeMap,
        treeMap,
        from ++ oldOwners,
        to ++ newOwners,
        from ++ substFrom,
        to ++ substTo)
    }

  /** Apply `typeMap` and `ownerMap` to given symbols `syms`
   *  and return a treemap that contains the substitution
   *  between original and mapped symbols.
   */
  def withMappedSyms(syms: List[Symbol], mapAlways: Boolean = false): TreeTypeMap =
    withMappedSyms(syms, ctx.mapSymbols(syms, this, mapAlways))

  /** The tree map with the substitution between originals `syms`
   *  and mapped symbols `mapped`. Also goes into mapped classes
   *  and substitutes their declarations.
   */
  def withMappedSyms(syms: List[Symbol], mapped: List[Symbol]): TreeTypeMap = {
    val symsChanged = syms ne mapped
    val substMap = withSubstitution(syms, mapped)
    val fullMap = (substMap /: mapped.filter(_.isClass)) { (tmap, cls) =>
      val origDcls = cls.info.decls.toList
      val mappedDcls = ctx.mapSymbols(origDcls, tmap)
      val tmap1 = tmap.withMappedSyms(origDcls, mappedDcls)
      if (symsChanged) (origDcls, mappedDcls).zipped.foreach(cls.asClass.replace)
      tmap1
    }
    if (symsChanged || (fullMap eq substMap)) fullMap
    else withMappedSyms(syms, mapAlways = true)
  }
}
