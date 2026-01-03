/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.generic

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.collection.{Factory, Iterable}
import scala.collection.mutable.Builder

/** The default serialization proxy for collection implementations.
  *
  * This class is `final` and requires an extra `Factory` object rather than leaving the details of creating a `Builder`
  * to an abstract method that could be implemented by a subclass. This is necessary because the factory is needed
  * for deserializing this class's private state, which happens before any subclass fields would be deserialized. Any
  * additional state required to create the proper `Builder` needs to be captured by the `factory`.
  */
@SerialVersionUID(3L)
final class DefaultSerializationProxy[A](factory: Factory[A, Any], @transient private val coll: Iterable[A]^) extends Serializable {

  @transient protected var builder: Builder[A, Any] = compiletime.uninitialized

  private def writeObject(out: ObjectOutputStream): Unit = {
    out.defaultWriteObject()
    val k = coll.knownSize
    out.writeInt(k)
    var count = 0
    coll.foreach { x =>
      out.writeObject(x)
      count += 1
    }
    if(k >= 0) {
      if(count != k) throw new IllegalStateException(s"Illegal size $count of collection, expected $k")
    } else out.writeObject(SerializeEnd)
  }

  private def readObject(in: ObjectInputStream): Unit = {
    in.defaultReadObject()
    builder = factory.newBuilder
    val k = in.readInt()
    if(k >= 0) {
      builder.sizeHint(k)
      var count = 0
      while(count < k) {
        builder += in.readObject().asInstanceOf[A]
        count += 1
      }
    } else {
      while (true) in.readObject match {
        case SerializeEnd => return
        case a => builder += a.asInstanceOf[A]
      }
    }
  }

  protected def readResolve(): Any = builder.result()
}

@SerialVersionUID(3L)
private[collection] case object SerializeEnd

/** Mix-in trait to enable DefaultSerializationProxy for the standard collection types. Depending on the type
  * it is mixed into, it will dynamically choose `iterableFactory`, `mapFactory`, `sortedIterableFactory` or
  * `sortedMapFactory` for deserialization into the respective `CC` type. Override `writeReplace` or implement
  * it directly without using this trait if you need a non-standard factory or if you want to use a different
  * serialization scheme.
  */
transparent trait DefaultSerializable extends Serializable { this: scala.collection.Iterable[?]^ =>
  protected def writeReplace(): AnyRef^{this} = {
    val f: Factory[Any, Any] = this match {
      case it: scala.collection.SortedMap[?, ?] => it.sortedMapFactory.sortedMapFactory[Any, Any](using it.ordering.asInstanceOf[Ordering[Any]]).asInstanceOf[Factory[Any, Any]]
      case it: scala.collection.Map[?, ?] => it.mapFactory.mapFactory[Any, Any].asInstanceOf[Factory[Any, Any]]
      case it: scala.collection.SortedSet[?] => it.sortedIterableFactory.evidenceIterableFactory[Any](using it.ordering.asInstanceOf[Ordering[Any]])
      case it => it.iterableFactory.iterableFactory
    }
    new DefaultSerializationProxy(f, this)
  }
}
