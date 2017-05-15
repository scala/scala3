package dotty.tools
package dottydoc
package util

import dotc.config.Printers.dottydoc
import dotc.core.Contexts.Context
import dotc.core.Flags
import dotc.core.Names._
import dotc.core.Symbols._
import dotc.core.Names._
import dotc.util.Positions._
import model.internal._
import model.comment._
import model._

trait MemberLookup {
  /** Performs a lookup based on the provided (pruned) query string
   *
   *  Will return a `Tooltip` if unsucessfull, otherwise a LinkToEntity or
   *  LinkToExternal
   */
  def lookup(entity: Entity, packages: Map[String, Package], query: String): Option[Entity] = {
    val notFound: Option[Entity] = None
    val querys = query.split("\\.").toList

    /** Looks for the specified entity among `ent`'s members */
    def localLookup(ent: Entity with Members, searchStr: String): Option[Entity] =
      ent
        .members
        .collect { case x if x.name == searchStr => x }
        .sortBy(_.path.last)
        .headOption

    /** Looks for an entity down in the structure, if the search list is Nil,
     *  the search stops
     */
    def downwardLookup(ent: Entity with Members, search: List[String]): Option[Entity] =
      search match {
        case Nil => notFound
        case x :: Nil =>
          localLookup(ent, x)
        case x :: xs  =>
          ent
            .members
            .collect {
              case e: Entity with Members if e.name == x => e
              case e: Entity with Members if e.name == x.init && x.last == '$' => e
            }
            .headOption
            .fold(notFound)(e => downwardLookup(e, xs))
      }

    /** Finds package with longest matching name, then does downwardLookup in
     *  the package
     */
    def globalLookup: Option[Entity] = {
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

    (querys, entity) match {
      case (xs, NonEntity) => globalLookup
      case (x :: Nil, e: Entity with Members) =>
        localLookup(e, x)
      case (x :: _, e: Entity with Members) if x == entity.name =>
        downwardLookup(e, querys)
      case (x :: xs, _) =>
        if (xs.nonEmpty) globalLookup
        else lookup(entity, packages, "scala." + query)
      case (Nil, _) =>
        throw new IllegalArgumentException("`query` cannot be empty")
    }
  }

  def makeEntityLink(
    entity: Entity,
    packages: Map[String, Package],
    title: Inline,
    query: String
  ): EntityLink = {
    val link =
      lookup(entity, packages, query)
      .map(LinkToEntity)
      .getOrElse(Tooltip(query))

    EntityLink(title, link)
  }
}
