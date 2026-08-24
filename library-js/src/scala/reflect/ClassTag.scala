/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
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

import scala.annotation.unchecked.uncheckedStable

import java.lang.{ Class => jClass }

import scala.collection.mutable
import scala.runtime.BoxedUnit

/** A `ClassTag[T]` stores the erased class of a given type `T`, accessible via the `runtimeClass`
 *  field. This is particularly useful for instantiating `Array`s whose element types are unknown
 *  at compile time.
 *
 *  `ClassTag`s are a weaker special case of [[scala.reflect.api.TypeTags#TypeTag]]s, in that they
 *  wrap only the runtime class of a given type, whereas a `TypeTag` contains all static type
 *  information. That is, `ClassTag`s are constructed from knowing only the top-level class of a
 *  type, without necessarily knowing all of its argument types. This runtime information is enough
 *  for runtime `Array` creation.
 *
 *  For example:
 *  ```
 *   scala> def mkArray[T : ClassTag](elems: T*) = Array[T](elems: _*)
 *   mkArray: [T](elems: T*)(implicit evidence$1: scala.reflect.ClassTag[T])Array[T]
 *
 *   scala> mkArray(42, 13)
 *   res0: Array[Int] = Array(42, 13)
 *
 *   scala> mkArray("Japan","Brazil","Germany")
 *   res1: Array[String] = Array(Japan, Brazil, Germany)
 *  ```
 *
 *  See [[scala.reflect.api.TypeTags]] for more examples, or the
 *  [Reflection Guide: TypeTags](http://docs.scala-lang.org/overviews/reflection/typetags-manifests.html)
 *  for more details.
 */
@scala.annotation.implicitNotFound(msg = "No ClassTag available for ${T}")
trait ClassTag[T] extends ClassManifestDeprecatedApis[T] with Equals with Serializable {
  // please, don't add any APIs here, like it was with `newWrappedArray` and `newArrayBuilder`
  // class tags, and all tags in general, should be as minimalistic as possible

  /** A class representing the type `U` to which `T` would be erased.
   *  Note that there is no subtyping relationship between `T` and `U`.
   */
  def runtimeClass: jClass[_]

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
  /** Tests whether `x` can possibly equal this class tag: only `ClassTag` instances can.
   *
   *  @param x the value to test
   */
  override def canEqual(x: Any) = x.isInstanceOf[ClassTag[_]]
  /** Tests whether `x` is a `ClassTag` whose `runtimeClass` equals this one's.
   *
   *  @param x the value to compare against
   */
  override def equals(x: Any) = x.isInstanceOf[ClassTag[_]] && this.runtimeClass == x.asInstanceOf[ClassTag[_]].runtimeClass
  /** Returns the hash code of `runtimeClass`, so that class tags that compare equal
   *  hash alike.
   */
  override def hashCode = runtimeClass.##
  /** Returns the name of `runtimeClass`, rendering array classes as `Array[...]` of
   *  their component type, recursively.
   */
  override def toString = {
    def prettyprint(clazz: jClass[_]): String =
      if (clazz.isArray) s"Array[${prettyprint(clazz.getComponentType)}]" else
      clazz.getName
    prettyprint(runtimeClass)
  }
}

/** Class tags corresponding to primitive types and constructor/extractor for ClassTags. */
object ClassTag {
  import ManifestFactory._

  /** The class tag for the value type `Byte`, whose `runtimeClass` is `java.lang.Byte.TYPE`. */
  val Byte    : ByteManifest               = ManifestFactory.Byte
  /** The class tag for the value type `Short`, whose `runtimeClass` is `java.lang.Short.TYPE`. */
  val Short   : ShortManifest              = ManifestFactory.Short
  /** The class tag for the value type `Char`, whose `runtimeClass` is `java.lang.Character.TYPE`. */
  val Char    : CharManifest               = ManifestFactory.Char
  /** The class tag for the value type `Int`, whose `runtimeClass` is `java.lang.Integer.TYPE`. */
  val Int     : IntManifest                = ManifestFactory.Int
  /** The class tag for the value type `Long`, whose `runtimeClass` is `java.lang.Long.TYPE`. */
  val Long    : LongManifest               = ManifestFactory.Long
  /** The class tag for the value type `Float`, whose `runtimeClass` is `java.lang.Float.TYPE`. */
  val Float   : FloatManifest              = ManifestFactory.Float
  /** The class tag for the value type `Double`, whose `runtimeClass` is `java.lang.Double.TYPE`. */
  val Double  : DoubleManifest             = ManifestFactory.Double
  /** The class tag for the value type `Boolean`, whose `runtimeClass` is `java.lang.Boolean.TYPE`. */
  val Boolean : BooleanManifest            = ManifestFactory.Boolean
  /** The class tag for the value type `Unit`, whose `runtimeClass` is `java.lang.Void.TYPE`. */
  val Unit    : UnitManifest               = ManifestFactory.Unit
  /** The class tag for the type `Any`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val Any     : ClassTag[scala.Any]        = ManifestFactory.Any
  /** The class tag for the type `Object`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val Object  : ClassTag[java.lang.Object] = ManifestFactory.Object
  /** The class tag for the type `AnyVal`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val AnyVal  : ClassTag[scala.AnyVal]     = ManifestFactory.AnyVal
  /** The class tag for the type `AnyRef`, the same instance as `Object`. */
  val AnyRef  : ClassTag[scala.AnyRef]     = ManifestFactory.AnyRef
  /** The class tag for the type `Nothing`, whose `runtimeClass` is `classOf[scala.runtime.Nothing$]`. */
  val Nothing : ClassTag[scala.Nothing]    = ManifestFactory.Nothing
  /** The class tag for the type `Null`, whose `runtimeClass` is `classOf[scala.runtime.Null$]`. */
  val Null    : ClassTag[scala.Null]       = ManifestFactory.Null

  @inline
  @SerialVersionUID(1L)
  private class GenericClassTag[T](val runtimeClass: jClass[_]) extends ClassTag[T] {
    /** Returns a new array with element type `T` and length `len`, created
     *  reflectively from `runtimeClass`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int): Array[T] = {
      java.lang.reflect.Array.newInstance(runtimeClass, len).asInstanceOf[Array[T]]
    }
  }

  /** Returns a `ClassTag[T]` for the given runtime class.
   *
   *  For the primitive classes (`java.lang.Byte.TYPE`, ..., `java.lang.Void.TYPE`) and
   *  for `classOf[java.lang.Object]`, `classOf[scala.runtime.Nothing$]`, and
   *  `classOf[scala.runtime.Null$]`, returns the corresponding shared tag
   *  (`ClassTag.Byte`, ..., `ClassTag.Unit`, `ClassTag.Object`, `ClassTag.Nothing`,
   *  `ClassTag.Null`); otherwise returns a new `ClassTag` wrapping `runtimeClass1`.
   *
   *  @tparam T the type whose erasure is `runtimeClass1`
   *  @param runtimeClass1 the runtime class of the type `T`
   */
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

  /** Extractor that yields the runtime class of a class tag.
   *
   *  @tparam T the type represented by `ctag`
   *  @param ctag the class tag to extract from
   *  @return `Some` of `ctag`'s `runtimeClass`
   */
  def unapply[T](ctag: ClassTag[T]): Option[Class[_]] = Some(ctag.runtimeClass)
}
