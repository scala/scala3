package dotty.dokka
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

      val res =
        def toplevelLookup(querystrings: List[String]) =
          downwardLookup(querystrings, defn.PredefModule.moduleClass)
          .orElse(downwardLookup(querystrings, defn.ScalaPackage))
          .orElse(downwardLookup(querystrings, defn.RootPackage))

        ownerOpt match {
          case Some(owner) =>
            val nearest = nearestMembered(owner)
            val nearestCls = nearestClass(owner)
            val nearestPkg = nearestPackage(owner)
            def relativeLookup(querystrings: List[String]) =
              // TODO walk the owner chain?
              downwardLookup(querystrings, nearestPkg).orElse(toplevelLookup(querystrings))
            query match {
              case Query.StrictMemberId(id) => localLookup(id, nearest).map(_ -> id)
              case Query.Id(id) =>
                (localLookup(id, nearest) orElse relativeLookup(List(id))).map(_ -> id)
              case Query.QualifiedId(Query.Qual.This, _, rest) =>
                downwardLookup(rest.asList, nearestCls).map(_ -> rest.join)
              case Query.QualifiedId(Query.Qual.Package, _, rest) =>
                downwardLookup(rest.asList, nearestPkg).map(_ -> rest.join)
              case Query.QualifiedId(Query.Qual.Id(id), _, rest) if id == nearestCls.name =>
                downwardLookup(rest.asList, nearestCls).map(_ -> rest.join)
              case Query.QualifiedId(Query.Qual.Id(id), _, rest) if id == nearestPkg.name =>
                downwardLookup(rest.asList, nearestPkg).map(_ -> rest.join)
              case query: Query.QualifiedId =>
                relativeLookup(query.asList).map(_ -> query.join)
            }

          case None =>
            toplevelLookup(query.asList).map(_ -> query.join)
        }

      // println(s"looked up `$query` in ${owner.show}[${owner.flags.show}] as ${res.map(_.show)}")

      res
    catch
      case e: Exception =>
        // TODO (https://github.com/lampepfl/scala3doc/issues/238): proper reporting
        println(s"[WARN] Unable to find a link for ${query} ${ownerOpt.fold("")(o => "in " + o.name)}")
        None

  private def hackMembersOf(using Quotes)(rsym: quotes.reflect.Symbol) = {
    import quotes.reflect._
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    val members = sym.info.decls.iterator.filter(_.isCompleted)
    // println(s"members of ${sym.show} : ${members.map(_.show).mkString(", ")}")
    members.asInstanceOf[Iterator[Symbol]]
  }

  private def hackIsNotAbsent(using Quotes)(rsym: quotes.reflect.Symbol) = {
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    sym.isCompleted
  }

  private def localLookup(using Quotes)(query: String, owner: quotes.reflect.Symbol): Option[quotes.reflect.Symbol] = {
    import quotes.reflect._

    inline def whenExists(s: Symbol)(otherwise: => Option[Symbol]): Option[Symbol] =
      if s.exists then Some(s) else otherwise

    def findMatch(syms: Iterator[Symbol]): Option[Symbol] = {
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
      // val matched0 = syms0.find(matches)
      // if matched0.isEmpty then
      //   println(s"Failed to look up $q in $owner; all members below:")
      //   syms0.foreach { s => println(s"\t$s") }
      // val matched = matched0

      // def showMatched() = matched.foreach { s =>
      //   println(s">>> ${s.show}")
      //   println(s">>> ${s.pos}")
      //   println(s">>> [${s.flags.show}]")
      //   println(s">>> {${if s.isTerm then "isterm" else ""};${if s.isType then "istype" else ""}}")
      //   println(s">>> moduleClass = ${if hackResolveModule(s) == s then hackResolveModule(s).show else "none"}")
      // }
      // println(s"localLookup for class ${owner.show} of `$q`{forceTerm=$forceTerm}")
      // showMatched()

      val matched = syms.find(matches)
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

          tpe.classSymbol.flatMap { s =>
            findMatch(hackMembersOf(s))
          }
        case _ =>
          findMatch(hackMembersOf(owner))
      }
  }

  private def downwardLookup(using Quotes)(query: List[String], owner: quotes.reflect.Symbol): Option[quotes.reflect.Symbol] =
    query match {
      case Nil => None
      case q :: Nil => localLookup(q, owner)
      case q :: qs => localLookup(q, owner).flatMap(downwardLookup(qs, _))
    }
}

object MemberLookup extends MemberLookup
