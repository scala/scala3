package bson

trait BSONWriter[T]
trait BSONDocumentWriter[T] extends BSONWriter[T]
object BSONWriter extends BSONWriterInstances

trait BSONHandler[T] extends BSONWriter[T]

private[bson] trait BSONWriterInstances {
  given mapWriter[V](using BSONWriter[V]): BSONDocumentWriter[Map[String, V]] = bson.mapWriter[V]
  export bson.collectionWriter
}

final class ¬[A, B]
object ¬ {
  implicit def defaultEvidence[A, B]: ¬[A, B] = new ¬[A, B]()
  @annotation.implicitAmbiguous("Could not prove type ${A} is not (¬) ${A}")
  implicit def ambiguousEvidence1[A]: ¬[A, A] = null
  implicit def ambiguousEvidence2[A]: ¬[A, A] = null
}

private[bson] trait DefaultBSONHandlers extends LowPriorityHandlers
private[bson] trait LowPriorityHandlers{
  given collectionWriter[T, Repr <: Iterable[T]](using BSONWriter[T], Repr ¬ Option[T]): BSONWriter[Repr] = ???
  private[bson] def mapWriter[V](implicit valueWriter: BSONWriter[V]): BSONDocumentWriter[Map[String, V]] = ???
}

// ---
package object bson extends DefaultBSONHandlers