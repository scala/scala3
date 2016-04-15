package dotty.tools
package dottydoc
package util

import dotc.config.Printers.dottydoc
import dotc.core.Contexts.Context
import dotc.core.Flags
import dotc.core.Names._
import dotc.core.Symbols._
import dotc.core.Types._
import dotc.core.Names._
import dotc.util.Positions._
import model.comment._
import model.Entities._

trait MemberLookup {
  def lookup(
    entity: Entity,
    packages: Map[String, Package],
    query: String,
    pos: Position
  ): LinkTo = {
    val notFound: LinkTo = Tooltip(query)
    val querys = query.split("\\.").toList

    def localLookup(ent: Entity with Members, searchStr: String): LinkTo =
      ent.members.find(_.name == searchStr).fold(notFound)(e => LinkToEntity(e))

    def downwardLookup(ent: Entity with Members, search: List[String]): LinkTo =
      search match {
        case Nil => notFound
        case x :: Nil =>
          localLookup(ent, x)
        case x :: xs  =>
          ent
            .members
            .collect { case e: Entity with Members => e }
            .find(_.name == x)
            .fold(notFound)(e => downwardLookup(e, xs))
      }

    def globalLookup: LinkTo = {
      def longestMatch(list: List[String]): List[String] =
        if (list == Nil) Nil
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
      case (x :: Nil, e: Entity with Members) =>
        localLookup(e, x)
      case (x :: _, e: Entity with Members) if x == entity.name =>
        downwardLookup(e, querys)
      case _ =>
        globalLookup
    }
  }

  def makeEntityLink(
    entity: Entity,
    packages: Map[String, Package],
    title: Inline,
    pos: Position,
    query: String
  ): Inline = EntityLink(title, lookup(entity, packages, query, pos))
}
