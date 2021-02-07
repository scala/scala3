package dotty.tools.scaladoc
package tasty.comments

import scala.quoted._

trait MemberLookup {

  def lookup(using Quotes)(
    query: Query,
    owner: quotes.reflect.Symbol,
  ): Option[(quotes.reflect.Symbol, String)] = lookupOpt(query, Some(owner))

  def lookupOpt(using Quotes)(
    query: Query,
    ownerOpt: Option[quotes.reflect.Symbol],
  ): Option[(quotes.reflect.Symbol, String)] =
    try
      import quotes.reflect._

      def nearestClass(sym: Symbol): Symbol =
        if sym.isClassDef then sym else nearestClass(sym.owner)

      def nearestPackage(sym: Symbol): Symbol =
        if sym.flags.is(Flags.Package) then sym else nearestPackage(sym.owner)

      def nearestMembered(sym: Symbol): Symbol =
        if sym.isClassDef || sym.flags.is(Flags.Package) then sym else nearestMembered(sym.owner)

      val res: Option[(Symbol, String)] = {
        def toplevelLookup(querystrings: List[String]) =
          downwardLookup(querystrings, defn.PredefModule.moduleClass)
          .orElse(downwardLookup(querystrings, defn.ScalaPackage))
          .orElse(downwardLookup(querystrings, defn.RootPackage))
          .orElse(downwardLookup(querystrings, defn.EmptyPackageClass))

        ownerOpt match {
          case Some(owner) =>
            val nearest = nearestMembered(owner)
            val nearestCls = nearestClass(owner)
            val nearestPkg = nearestPackage(owner)
            def relativeLookup(querystrings: List[String], owner: Symbol): Option[Symbol] = {
              val isMeaningful =
                owner.exists
                // those are just an optimisation, they can be dropped if problems show up
                && owner.ne(defn.ScalaPackage)
                && owner.ne(defn.RootClass)
                && owner.ne(defn.EmptyPackageClass)

              if !isMeaningful then None else {
                downwardLookup(querystrings, owner) match {
                  case None => relativeLookup(querystrings, owner.owner)
                  case some => some
                }
              }
            }

            query match {
              case Query.StrictMemberId(id) =>
                localLookup(id, nearest).nextOption.map(_ -> id)
              case Query.QualifiedId(Query.Qual.This, _, rest) =>
                downwardLookup(rest.asList, nearestCls).map(_ -> rest.join)
              case Query.QualifiedId(Query.Qual.Package, _, rest) =>
                downwardLookup(rest.asList, nearestPkg).map(_ -> rest.join)
              case query =>
                val ql = query.asList
                toplevelLookup(ql)
                .orElse(relativeLookup(ql, nearest))
                .map(_ -> query.join)
            }

          case None =>
            toplevelLookup(query.asList).map(_ -> query.join)
        }
      }

      // println(s"looked up `$query` in ${owner.show}[${owner.flags.show}] as ${res.map(_.show)}")

      res
    catch
      case e: Exception =>
        // TODO (https://github.com/lampepfl/scala3doc/issues/238): proper reporting
        println(s"[WARN] Unable to find a link for ${query} ${ownerOpt.fold("")(o => "in " + o.name)}")
        e.printStackTrace()
        None

  private def hackMembersOf(using Quotes)(rsym: quotes.reflect.Symbol) = {
    import quotes.reflect._
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    val members = sym.info.decls.iterator.filter(s => hackIsNotAbsent(s.asInstanceOf[Symbol]))
    // println(s"members of ${sym.show} : ${members.map(_.show).mkString(", ")}")
    members.asInstanceOf[Iterator[Symbol]]
  }

  private def hackIsNotAbsent(using Quotes)(rsym: quotes.reflect.Symbol) = {
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    // note: Predef has .info = NoType for some reason
    sym.isCompleted && sym.info.exists
  }

  private def localLookup(using Quotes)(query: String, owner: quotes.reflect.Symbol): Iterator[quotes.reflect.Symbol] = {
    import quotes.reflect._

    def findMatch(syms: Iterator[Symbol]): Iterator[Symbol] = {
      // Scaladoc overloading support allows terminal * (and they're meaningless)
      val cleanQuery = query.stripSuffix("*")
      val (q, forceTerm, forceType) =
        if cleanQuery endsWith "$" then
          (cleanQuery.init, true, false)
        else if cleanQuery endsWith "!" then
          (cleanQuery.init, false, true)
        else
          (cleanQuery, false, false)

      def matches(s: Symbol): Boolean =
        s.name == q && (
          if forceTerm then s.isTerm
          else if forceType then s.isType
          else true
        )

      def hackResolveModule(s: Symbol): Symbol =
        if s.flags.is(Flags.Module) then s.moduleClass else s

      // val syms0 = syms.toList
      // val matched0 = syms0.filter(matches)
      // if matched0.isEmpty then
      //   println(s"Failed to look up $q in $owner; all members below:")
      //   syms0.foreach { s => println(s"\t$s") }
      // val matched = matched0.iterator

      // def showMatched() = matched.foreach { s =>
      //   println(s">>> $s")
      //   println(s">>> ${s.pos}")
      //   println(s">>> [${s.flags.show}]")
      //   println(s">>> {${if s.isTerm then "isterm" else ""};${if s.isType then "istype" else ""}}")
      //   println(s">>> moduleClass = ${if hackResolveModule(s) == s then hackResolveModule(s).show else "none"}")
      // }
      // println(s"localLookup in class ${owner} for `$q`{forceTerm=$forceTerm}")
      // println(s"\t${matched0.mkString(", ")}")
      // showMatched()

      val matched = syms.filter(matches)
      matched.map(hackResolveModule)
    }

    if owner.isPackageDef then
      findMatch(hackMembersOf(owner))
    else
      owner.tree match {
        case tree: ClassDef =>
          findMatch(tree.body.iterator.collect { case t: Definition if hackIsNotAbsent(t.symbol) => t.symbol })
        case tree: TypeDef =>
          val tpe =
            tree.rhs match {
              case tb : TypeBoundsTree => tb.hi.tpe
              case tpt: TypeTree => tpt.tpe
            }

          tpe.classSymbol match {
            case Some(s) => findMatch(hackMembersOf(s))
            case None => Iterator.empty
          }
        case _ =>
          findMatch(hackMembersOf(owner))
      }
  }

  private def downwardLookup(using Quotes)(query: List[String], owner: quotes.reflect.Symbol): Option[quotes.reflect.Symbol] = {
    import quotes.reflect._
    query match {
      case Nil => None
      case q :: Nil => localLookup(q, owner).nextOption
      case q :: qs =>
        val lookedUp =
          localLookup(q, owner).toSeq

        if lookedUp.isEmpty then None else {
          // tm/tp - term/type symbols which we looked up and which allow further lookup
          // pk - package symbol
          // Note: packages collide with both term and type definitions
          // Note: classes and types collide
          var pk: Option[Symbol] = None
          var tp: Option[Symbol] = None
          var tm: Option[Symbol] = None
          lookedUp.foreach { s =>
            if s.isPackageDef then pk = Some(s)
            else if s.flags.is(Flags.Module) then tm = Some(s)
            else if s.isClassDef || s.isTypeDef then tp = Some(s)
          }
          pk.flatMap(downwardLookup(qs, _))
          .orElse(tp.flatMap(downwardLookup(qs, _)))
          .orElse(tm.flatMap(downwardLookup(qs, _)))
        }
    }
  }
}

object MemberLookup extends MemberLookup
