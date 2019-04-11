package dotty.tastydoc
package comment
package util

import dotty.tastydoc.representations._

trait MemberLookup {
  /** Performs a lookup based on the provided (pruned) query string
   *
   *  Will return a `Tooltip` if unsuccessful, otherwise a LinkToRepresentation or
   *  LinkToExternal
   */
  def lookup(Representation: Option[Representation], packages: Map[String, PackageRepresentation], query: String): Option[Representation] = {
    val notFound: Option[Representation] = None
    val querys = query.split("\\.").toList

    /** Looks for the specified Representation among `ent`'s members */
    def localLookup(ent: Representation with Members, searchStr: String): Option[Representation] =
      ent
        .members
        .collect { case x if x.name == searchStr => x }
        .sortBy(_.path.last)
        .headOption

    /** Looks for an Representation down in the structure, if the search list is Nil,
     *  the search stops
     */
    def downwardLookup(ent: Representation with Members, search: List[String]): Option[Representation] =
      search match {
        case Nil => notFound
        case x :: Nil =>
          localLookup(ent, x)
        case x :: xs  =>
          ent
            .members
            .collectFirst {
              case e: Representation with Members if e.name == x => e
              case e: Representation with Members if e.name == x.init && x.last == '$' => e
            }
            .fold(notFound)(e => downwardLookup(e, xs))
      }

    /** Finds package with longest matching name, then does downwardLookup in
     *  the package
     */
    def globalLookup: Option[Representation] = {
      def longestMatch(list: List[String]): List[String] =
        if (list eq Nil) Nil
        else
          packages
          .get(list.mkString("."))
          .map(_ => list)
          .getOrElse(longestMatch(list.dropRight(1)))

      longestMatch(querys) match {
        case Nil => notFound
        case xs  => downwardLookup(packages(xs.mkString(".")), querys diff xs)
      }
    }

    (querys, Representation) match {
      case (xs, None) => globalLookup
      case (x :: Nil, Some(e: Representation with Members)) =>
        localLookup(e, x)
      case (x :: _, Some(e: Representation with Members)) if x == e.name =>
        downwardLookup(e, querys)
      case (x :: xs, _) =>
        if (xs.nonEmpty) globalLookup
        else lookup(Representation, packages, "scala." + query)
      case (Nil, _) =>
        throw new IllegalArgumentException("`query` cannot be empty")
    }
  }

  def makeRepresentationLink(
    Representation: Representation,
    packages: Map[String, PackageRepresentation],
    title: Inline,
    query: String
  ): RepresentationLink = {
    val link =
      lookup(Some(Representation), packages, query)
      .map(LinkToRepresentation)
      .getOrElse(Tooltip(query))

    RepresentationLink(title, link)
  }
}
