package dotty.dokka.tasty.comments

import scala.tasty.Reflection

case class RepresentationLink(
  title: String,
  link: Link
)

enum Link {
  case Tooltip(query: String)
}
export Link._

trait MemberLookup {

  /** Performs a lookup based on the provided (pruned) query string
   *
   *  Will return a `Tooltip` if unsuccessful, otherwise a LinkToRepresentation
   *  or LinkToExternal.
   *
   *
   */
  def makeRepresentationLink(
    representation: Repr,
    packages: Packages,
    title: String,
    query: String
  ): RepresentationLink = {
    // val link =
    //   lookup(Some(representation), packages, query)
    //   .map(LinkToRepresentation)
    //   .getOrElse(Tooltip(query))

    val link = Tooltip(query)

    RepresentationLink(title, link)
  }

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

  // def lookup(
  //   representation: Option[Representation],
  //   packages: Map[String, EmulatedPackageRepresentation],
  //   query: String
  // ): Option[Representation] = {
  //   val notFound: Option[Representation] = None
  //   val querys = query.split("\\.").toList

  //   /** Looks for the specified Representation among `ent`'s members */
  //   def localLookup(ent: Representation with Members, searchStr: String): Option[Representation] =
  //     ent
  //       .members
  //       .collect { case x if x.name == searchStr => x }
  //       .sortBy(_.path.last)
  //       .headOption

  //   /** Looks for a Representation down in the structure, if the search list is Nil,
  //    *  the search stops
  //    */
  //   def downwardLookup(ent: Representation with Members, search: List[String]): Option[Representation] =
  //     search match {
  //       case Nil => notFound
  //       case x :: Nil =>
  //         localLookup(ent, x)
  //       case x :: xs  =>
  //         ent
  //           .members
  //           .collectFirst {
  //             case e: Representation with Members if e.name == x => e
  //             case e: Representation with Members if e.name == x.init && x.last == '$' => e
  //           }
  //           .fold(notFound)(e => downwardLookup(e, xs))
  //     }

  //   /** Finds package with longest matching name, then does downwardLookup in
  //    *  the package
  //    */
  //   def globalLookup: Option[Representation] = {
  //     def longestMatch(list: List[String]): List[String] =
  //       if (list eq Nil) Nil
  //       else
  //         packages
  //         .get(list.mkString("."))
  //         .map(_ => list)
  //         .getOrElse(longestMatch(list.dropRight(1)))

  //     longestMatch(querys) match {
  //       case Nil => notFound
  //       case xs  => downwardLookup(packages(xs.mkString(".")), querys diff xs)
  //     }
  //   }

  //   (querys, representation) match {
  //     case (xs, None) => globalLookup
  //     case (x :: Nil, Some(e: Representation with Members)) =>
  //       localLookup(e, x)
  //     case (x :: _, Some(e: Representation with Members)) if x == e.name =>
  //       downwardLookup(e, querys)
  //     case (x :: xs, _) =>
  //       if (xs.nonEmpty) globalLookup
  //       else lookup(representation, packages, "scala." + query)
  //     case (Nil, _) =>
  //       throw new IllegalArgumentException("`query` cannot be empty")
  //   }
  // }
}

object MemberLookup extends MemberLookup
