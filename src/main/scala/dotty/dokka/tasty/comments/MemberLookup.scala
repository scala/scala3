package dotty.dokka.tasty.comments

import scala.tasty.Reflection

trait MemberLookup {

  def lookup(using r: Reflection)(
    query: String,
    owner: r.Symbol,
  ): Option[r.Symbol] = {
    val parsedQuery: List[String] = query.split("\\.").toList

    val res =
      parsedQuery match {
        case q :: Nil => localLookup(q, owner)
        case q :: qs if q == owner.name || q == "this" => downwardLookup(qs, owner)
        case q :: qs => downwardLookup(q :: qs, r.defn.RootPackage)
        case _ => None
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
      val (q, forceTerm) =
        if query endsWith "$" then (query.init, true) else (query, false)

      def matches(s: r.Symbol): Boolean =
        s.name == q && (if forceTerm then s.isTerm else true)

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
