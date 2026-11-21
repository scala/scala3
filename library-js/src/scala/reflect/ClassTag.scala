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

package scala
package reflect

import scala.language.`2.13`
import java.lang.{Class => jClass}
import scala.annotation.{implicitNotFound, nowarn}

/** A `ClassTag[T]` stores the erased class of a given type `T`, accessible via the `runtimeClass`
 *  field. This is particularly useful for instantiating `Array`s whose element types are unknown
 *  at compile time.
 *
 *  `ClassTag`s wrap only the runtime class of a given type, without necessarily knowing all of
 *  its argument types. This runtime information is enough for runtime `Array` creation.
 *
 *  For example:
 *  ```scala sc:compile
 *  import scala.reflect.ClassTag
 *
 *  def mkArray[T: ClassTag](elems: T*): Array[T] = Array[T](elems*)
 *
 *  val ints = mkArray(42, 13)
 *  val strings = mkArray("Japan", "Brazil", "Germany")
 *  ```
 *
 *  For compile-time type information in macros, see the facilities in the
 *  [[scala.quoted]] package.
 *  For limited runtime type checks beyond what `Class[?]` provides, see
 *  [[scala.reflect.TypeTest]] and [[scala.reflect.Typeable]].
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifestDeprecatedApis""")
@implicitNotFound(msg = "No reflect.ClassTag available for ${T}. You can fix this by changing [${T}] to [${T}: reflect.ClassTag].")
trait ClassTag[T] extends ClassManifestDeprecatedApis[T] with Equals with Serializable {
  // please, don't add any APIs here, like it was with `newWrappedArray` and `newArrayBuilder`
  // class tags, and all tags in general, should be as minimalistic as possible

  /** A class representing the type `U` to which `T` would be erased.
   *  Note that there is no subtyping relationship between `T` and `U`.
   */
  def runtimeClass: jClass[?]

  /** Produces a `ClassTag` that knows how to instantiate an `Array[Array[T]]`. */
  def wrap: ClassTag[Array[T]] = ClassTag[Array[T]](arrayClass(runtimeClass))

  /** Produces a new array with element type `T` and length `len`.
   *
   *  @param len the length of the new array
   */
  def newArray(len: Int): Array[T] =
    java.lang.reflect.Array.newInstance(runtimeClass, len).asInstanceOf[Array[T]]

  /** A ClassTag[T] can serve as an extractor that matches only objects of type T.
   *
   *  The compiler tries to turn unchecked type tests in pattern matches into checked ones
   *  by wrapping a `(_: T)` type pattern as `ct(_: T)`, where `ct` is the `ClassTag[T]` instance.
   *  Type tests necessary before calling other extractors are treated similarly.
   *  `SomeExtractor(...)` is turned into `ct(SomeExtractor(...))` if `T` in `SomeExtractor.unapply(x: T)`
   *  is uncheckable, but we have an instance of `ClassTag[T]`.
   *
   *  @param x the value to match against the runtime class of `T`
   *  @return `Some(x)` if `x` is an instance of `T`, `None` otherwise
   */
  def unapply(x: Any): Option[T] =
    if (runtimeClass.isInstance(x)) Some(x.asInstanceOf[T])
    else None

  // case class accessories
  override def canEqual(x: Any) = x.isInstanceOf[ClassTag[?]]
  override def equals(x: Any) = x.isInstanceOf[ClassTag[?]] && this.runtimeClass == x.asInstanceOf[ClassTag[?]].runtimeClass
  override def hashCode() = runtimeClass.##
  override def toString() = {
    def prettyprint(clazz: jClass[?]): String =
      if (clazz.isArray) s"Array[${prettyprint(clazz.getComponentType)}]" else
      clazz.getName
    prettyprint(runtimeClass)
  }
}

/** Class tags corresponding to primitive types and constructor/extractor for ClassTags. */
object ClassTag {
  import ManifestFactory._

  val Byte    : ByteManifest               = ManifestFactory.Byte
  val Short   : ShortManifest              = ManifestFactory.Short
  val Char    : CharManifest               = ManifestFactory.Char
  val Int     : IntManifest                = ManifestFactory.Int
  val Long    : LongManifest               = ManifestFactory.Long
  val Float   : FloatManifest              = ManifestFactory.Float
  val Double  : DoubleManifest             = ManifestFactory.Double
  val Boolean : BooleanManifest            = ManifestFactory.Boolean
  val Unit    : UnitManifest               = ManifestFactory.Unit
  val Any     : ClassTag[scala.Any]        = ManifestFactory.Any
  val Object  : ClassTag[java.lang.Object] = ManifestFactory.Object
  val AnyVal  : ClassTag[scala.AnyVal]     = ManifestFactory.AnyVal
  val AnyRef  : ClassTag[scala.AnyRef]     = ManifestFactory.AnyRef
  val Nothing : ClassTag[scala.Nothing]    = ManifestFactory.Nothing
  val Null    : ClassTag[scala.Null]       = ManifestFactory.Null

  @inline
  @SerialVersionUID(1L)
  private class GenericClassTag[T](val runtimeClass: jClass[?]) extends ClassTag[T] {
    override def newArray(len: Int): Array[T] = {
      java.lang.reflect.Array.newInstance(runtimeClass, len).asInstanceOf[Array[T]]
    }
  }

  def apply[T](runtimeClass1: jClass[_]): ClassTag[T] =
    runtimeClass1 match {
      case java.lang.Byte.TYPE      => ClassTag.Byte.asInstanceOf[ClassTag[T]]
      case java.lang.Short.TYPE     => ClassTag.Short.asInstanceOf[ClassTag[T]]
      case java.lang.Character.TYPE => ClassTag.Char.asInstanceOf[ClassTag[T]]
      case java.lang.Integer.TYPE   => ClassTag.Int.asInstanceOf[ClassTag[T]]
      case java.lang.Long.TYPE      => ClassTag.Long.asInstanceOf[ClassTag[T]]
      case java.lang.Float.TYPE     => ClassTag.Float.asInstanceOf[ClassTag[T]]
      case java.lang.Double.TYPE    => ClassTag.Double.asInstanceOf[ClassTag[T]]
      case java.lang.Boolean.TYPE   => ClassTag.Boolean.asInstanceOf[ClassTag[T]]
      case java.lang.Void.TYPE      => ClassTag.Unit.asInstanceOf[ClassTag[T]]
      case _                        =>
        if (classOf[java.lang.Object] == runtimeClass1)
          ClassTag.Object.asInstanceOf[ClassTag[T]]
        else if (classOf[scala.runtime.Nothing$] == runtimeClass1)
          ClassTag.Nothing.asInstanceOf[ClassTag[T]]
        else if (classOf[scala.runtime.Null$] == runtimeClass1)
          ClassTag.Null.asInstanceOf[ClassTag[T]]
        else
          new GenericClassTag[T](runtimeClass1)
    }

  def unapply[T](ctag: ClassTag[T]): Option[Class[?]] = Some(ctag.runtimeClass)
}
