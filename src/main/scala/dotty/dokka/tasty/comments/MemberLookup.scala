package dotty.dokka.tasty.comments

import scala.tasty.Reflection

trait MemberLookup {

  def lookup(using r: Reflection)(
    query: Query,
    owner: r.Symbol,
  ): Option[(r.Symbol, String)] = lookupOpt(query, Some(owner))

  def lookupOpt(using r: Reflection)(
    query: Query,
    ownerOpt: Option[r.Symbol],
  ): Option[(r.Symbol, String)] = {

    def nearestClass(sym: r.Symbol): r.Symbol =
      if sym.isClassDef then sym else nearestClass(sym.owner)

    def nearestPackage(sym: r.Symbol): r.Symbol =
      if sym.flags.is(r.Flags.Package) then sym else nearestPackage(sym.owner)

    def nearestMembered(sym: r.Symbol): r.Symbol =
      if sym.isClassDef || sym.flags.is(r.Flags.Package) then sym else nearestMembered(sym.owner)

    val res =
      ownerOpt match {
        case Some(owner) =>
          val nearest = nearestMembered(owner)
          val nearestCls = nearestClass(owner)
          val nearestPkg = nearestPackage(owner)
          query match {
            case Query.StrictMemberId(id) => localLookup(id, nearest).map(_ -> id)
            case Query.Id(id) =>
              (localLookup(id, nearest) orElse localLookup(id, nearestPkg)).map(_ -> id)
            case Query.QualifiedId(Query.Qual.This, _, rest) =>
              downwardLookup(rest.asList, nearestCls).map(_ -> rest.join)
            case Query.QualifiedId(Query.Qual.Package, _, rest) =>
              downwardLookup(rest.asList, nearestPkg).map(_ -> rest.join)
            case Query.QualifiedId(Query.Qual.Id(id), _, rest) if id == nearestCls.name =>
              downwardLookup(rest.asList, nearestCls).map(_ -> rest.join)
            case Query.QualifiedId(Query.Qual.Id(id), _, rest) if id == nearestPkg.name =>
              downwardLookup(rest.asList, nearestPkg).map(_ -> rest.join)
            case query: Query.QualifiedId => downwardLookup(query.asList, r.defn.RootPackage).map(_ -> query.join)
          }

        case None =>
          downwardLookup(query.asList, r.defn.RootPackage).map(_ -> query.join)
      }

    // println(s"looked up `$query` in ${owner.show}[${owner.flags.show}] as ${res.map(_.show)}")

    res
  }

  private def hackMembersOf(using r: Reflection)(rsym: r.Symbol) = {
    import dotty.tools.dotc
    given dotc.core.Contexts.Context = r.rootContext.asInstanceOf
    val sym = rsym.asInstanceOf[dotc.core.Symbols.Symbol]
    val members = sym.info.decls.iterator
    // println(s"members of ${sym.show} : ${members.map(_.show).mkString(", ")}")
    members.asInstanceOf[Iterator[r.Symbol]]
  }

  private def localLookup(using r: Reflection)(query: String, owner: r.Symbol): Option[r.Symbol] = {
    import r._

    inline def whenExists(s: Symbol)(otherwise: => Option[r.Symbol]): Option[r.Symbol] =
      if s.exists then Some(s) else otherwise

    def findMatch(syms: Iterator[r.Symbol]): Option[r.Symbol] = {
      // Scaladoc overloading support allows terminal * (and they're meaningless)
      val cleanQuery = query.stripSuffix("*")
      val (q, forceTerm, forceType) =
        if cleanQuery endsWith "$" then
          (cleanQuery.init, true, false)
        else if cleanQuery endsWith "!" then
          (cleanQuery.init, false, true)
        else
          (cleanQuery, false, false)

      def matches(s: r.Symbol): Boolean =
        s.name == q && (
          if forceTerm then s.isTerm
          else if forceType then s.isType
          else true
        )

      def hackResolveModule(s: r.Symbol): r.Symbol =
        if s.flags.is(Flags.Object) then s.moduleClass else s

      val matched = syms.find(matches)

      // def showMatched() = matched.foreach { s =>
      //   println(s">>> ${s.show}")
      //   println(s">>> ${s.pos}")
      //   println(s">>> [${s.flags.show}]")
      //   println(s">>> {${if s.isTerm then "isterm" else ""};${if s.isType then "istype" else ""}}")
      //   println(s">>> moduleClass = ${if hackResolveModule(s) == s then hackResolveModule(s).show else "none"}")
      // }
      // println(s"localLookup for class ${owner.show} of `$q`{forceTerm=$forceTerm}")
      // showMatched()

      matched.map(hackResolveModule)
    }

    owner.tree match {
      case tree: r.ClassDef =>
        findMatch(tree.body.iterator.collect { case t: r.Definition => t.symbol })
      case _ =>
        findMatch(hackMembersOf(owner))
    }
  }

  private def downwardLookup(using r: Reflection)(query: List[String], owner: r.Symbol): Option[r.Symbol] =
    query match {
      case Nil => None
      case q :: Nil => localLookup(q, owner)
      case q :: qs => localLookup(q, owner).flatMap(downwardLookup(qs, _))
    }
}

object MemberLookup extends MemberLookup
