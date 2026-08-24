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

import scala.collection.mutable.{ArrayBuilder, ArraySeq}

/** A `Manifest[T]` is an opaque descriptor for type T.  Its supported use
 *  is to give access to the erasure of the type as a `Class` instance, as
 *  is necessary for the creation of native `Arrays` if the class is not
 *  known at compile time.
 *
 *  The type-relation operators `<:<` and `=:=` should be considered
 *  approximations only, as there are numerous aspects of type conformance
 *  which are not yet adequately represented in manifests.
 *
 *  Example usages:
 *  ```
 *    def arr[T] = new Array[T](0)                          // does not compile
 *    def arr[T](implicit m: Manifest[T]) = new Array[T](0) // compiles
 *    def arr[T: Manifest] = new Array[T](0)                // shorthand for the preceding
 *
 *    // Methods manifest and optManifest are in [[scala.Predef]].
 *    def isApproxSubType[T: Manifest, U: Manifest] = manifest[T] <:< manifest[U]
 *    isApproxSubType[List[String], List[AnyRef]] // true
 *    isApproxSubType[List[String], List[Int]]    // false
 *
 *    def methods[T: Manifest] = manifest[T].runtimeClass.getMethods
 *    def retType[T: Manifest](name: String) =
 *      methods[T] find (_.getName == name) map (_.getGenericReturnType)
 *
 *    retType[Map[_, _]]("values")  // Some(scala.collection.Iterable<B>)
 *  ```
 */
@scala.annotation.implicitNotFound(msg = "No Manifest available for ${T}.")
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use scala.reflect.ClassTag (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
trait Manifest[T] extends ClassManifest[T] with Equals {
  /** Returns the manifests for the type arguments of the type represented by this
   *  manifest. This default implementation returns `Nil`; manifests built with type
   *  arguments override it.
   */
  override def typeArguments: List[Manifest[_]] = Nil

  /** Returns a manifest for the array type `Array[T]`, whose runtime class is the
   *  array class with this manifest's runtime class as component, and whose only
   *  type argument is this manifest.
   */
  override def arrayManifest: Manifest[Array[T]] =
    Manifest.classType[Array[T]](arrayClass[T](runtimeClass), this)

  /** Tests whether `that` can possibly equal this manifest.
   *
   *  @param that the value to test
   *  @return `true` if `that` is a `Manifest`, `false` otherwise
   */
  override def canEqual(that: Any): Boolean = that match {
    case _: Manifest[_]   => true
    case _                => false
  }
  /** Note: testing for erasure here is important, as it is many times
   *  faster than <:< and rules out most comparisons.
   *
   *  @param that the object to compare for equality
   */
  override def equals(that: Any): Boolean = that match {
    case m: Manifest[_] => (m canEqual this) && (this.runtimeClass == m.runtimeClass) && (this <:< m) && (m <:< this)
    case _              => false
  }
  /** Returns the hash code of `runtimeClass`, so that manifests that compare equal
   *  hash alike.
   */
  override def hashCode = this.runtimeClass.##
}

/** The object `Manifest` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 */
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use scala.reflect.ClassTag (to capture erasures), scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
object Manifest {
  /* Forward all the public members of ManifestFactory, since this object used
   * to be a `private val Manifest = ManifestFactory` in the package object. It
   * was moved here because it needs to be in the same file as `trait Manifest`
   * defined above.
   */

  /** Returns the manifests for the value types `Byte`, `Short`, `Char`, `Int`,
   *  `Long`, `Float`, `Double`, `Boolean`, and `Unit`, in that order.
   */
  def valueManifests: List[AnyValManifest[_]] =
    ManifestFactory.valueManifests

  /** The manifest for the value type `Byte`, whose `runtimeClass` is `java.lang.Byte.TYPE`. */
  val Byte: ManifestFactory.ByteManifest = ManifestFactory.Byte
  /** The manifest for the value type `Short`, whose `runtimeClass` is `java.lang.Short.TYPE`. */
  val Short: ManifestFactory.ShortManifest = ManifestFactory.Short
  /** The manifest for the value type `Char`, whose `runtimeClass` is `java.lang.Character.TYPE`. */
  val Char: ManifestFactory.CharManifest = ManifestFactory.Char
  /** The manifest for the value type `Int`, whose `runtimeClass` is `java.lang.Integer.TYPE`. */
  val Int: ManifestFactory.IntManifest = ManifestFactory.Int
  /** The manifest for the value type `Long`, whose `runtimeClass` is `java.lang.Long.TYPE`. */
  val Long: ManifestFactory.LongManifest = ManifestFactory.Long
  /** The manifest for the value type `Float`, whose `runtimeClass` is `java.lang.Float.TYPE`. */
  val Float: ManifestFactory.FloatManifest = ManifestFactory.Float
  /** The manifest for the value type `Double`, whose `runtimeClass` is `java.lang.Double.TYPE`. */
  val Double: ManifestFactory.DoubleManifest = ManifestFactory.Double
  /** The manifest for the value type `Boolean`, whose `runtimeClass` is `java.lang.Boolean.TYPE`. */
  val Boolean: ManifestFactory.BooleanManifest = ManifestFactory.Boolean
  /** The manifest for the value type `Unit`, whose `runtimeClass` is `java.lang.Void.TYPE`. */
  val Unit: ManifestFactory.UnitManifest = ManifestFactory.Unit

  /** The manifest for the type `Any`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val Any: Manifest[scala.Any] = ManifestFactory.Any
  /** The manifest for the type `Object`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val Object: Manifest[java.lang.Object] = ManifestFactory.Object
  /** The manifest for the type `AnyRef`, the same instance as `Object`. */
  val AnyRef: Manifest[scala.AnyRef] = ManifestFactory.AnyRef
  /** The manifest for the type `AnyVal`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val AnyVal: Manifest[scala.AnyVal] = ManifestFactory.AnyVal
  /** The manifest for the type `Null`, whose `runtimeClass` is `classOf[scala.runtime.Null$]`. */
  val Null: Manifest[scala.Null] = ManifestFactory.Null
  /** The manifest for the type `Nothing`, whose `runtimeClass` is `classOf[scala.runtime.Nothing$]`. */
  val Nothing: Manifest[scala.Nothing] = ManifestFactory.Nothing

  /** Manifest for the singleton type `value.type`.
   *
   *  @tparam T the type to manifest, constrained to `AnyRef` subtypes
   *  @param value the singleton instance for which to create a manifest
   */
  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] =
    ManifestFactory.singleType[T](value)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a top-level or static class.
   *  @note This no-prefix, no-arguments case is separate because we
   *       it's called from ScalaRunTime.boxArray itself. If we
   *       pass varargs as arrays into this, we get an infinitely recursive call
   *       to boxArray. (Besides, having a separate case is more efficient)
   *
   *  @tparam T the type to manifest
   *  @param clazz the runtime `Class` representing the erased type `T`
   */
  def classType[T](clazz: Predef.Class[_]): Manifest[T] =
    ManifestFactory.classType[T](clazz)

  /** Manifest for the class type `clazz`, where `clazz` is
   *  a top-level or static class and args are its type arguments. 
   *
   *  @tparam T the type to manifest
   *  @param clazz the runtime `Class` representing the erased type `T`
   *  @param arg1 the manifest for the first type argument
   *  @param args the manifests for the remaining type arguments
   */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[_], args: Manifest[_]*): Manifest[T] =
    ManifestFactory.classType[T](clazz, arg1, args: _*)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a class with non-package prefix type `prefix` and type arguments `args`.
   *
   *  @tparam T the type to manifest
   *  @param prefix the manifest for the enclosing type (non-package prefix)
   *  @param clazz the runtime `Class` representing the erased type `T`
   *  @param args the manifests for the type arguments
   */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    ManifestFactory.classType[T](prefix, clazz, args: _*)

  /** Manifest for the array type `Array[T]`, where `arg` manifests the element type `T`.
   *
   *  @tparam T the element type of the resulting array manifest; it is not checked against
   *            `arg`, so a mismatched pair yields a manifest whose represented element type
   *            differs from its static one
   *  @param arg the manifest for the element type
   *  @return the array manifest of `arg`
   */
  def arrayType[T](arg: Manifest[_]): Manifest[Array[T]] =
    ManifestFactory.arrayType[T](arg)

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
   *  strictly necessary as it could be obtained by reflection. It was
   *  added so that erasure can be calculated without reflection. 
   *
   *  @tparam T the type to manifest
   *  @param prefix the manifest for the type in which this abstract type is a member
   *  @param name the name of the abstract type member
   *  @param upperBound the `Class` representing the erasure of the abstract type's upper bound
   *  @param args the manifests for the type arguments
   */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    ManifestFactory.abstractType[T](prefix, name, upperBound, args: _*)

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
   *
   *  @tparam T the type to manifest
   *  @param lowerBound the manifest for the wildcard's lower bound `L`
   *  @param upperBound the manifest for the wildcard's upper bound `U`
   */
  def wildcardType[T](lowerBound: Manifest[_], upperBound: Manifest[_]): Manifest[T] =
    ManifestFactory.wildcardType[T](lowerBound, upperBound)

  /** Manifest for the intersection type `parents_0 with ... with parents_n`.
   *
   *  @tparam T the type to manifest
   *  @param parents the manifests for each type in the intersection
   */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    ManifestFactory.intersectionType[T](parents: _*)

}

// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use type tags and manually check the corresponding class or type instead", "2.10.0")
/** The base manifest for Scala's value types.
 *
 *  Value manifests compare with reference identity: a value manifest equals only
 *  itself, and represents a subtype (`<:<`) only of itself, `Any`, and `AnyVal`.
 *
 *  @tparam T the value type represented by this manifest
 *  @param toString the name of the value type, used as the string representation of this manifest
 */
@SerialVersionUID(1L)
abstract class AnyValManifest[T <: AnyVal](override val toString: String) extends Manifest[T] with Equals {
  /** Tests whether the type represented by this manifest is a subtype of the type
   *  represented by `that`.
   *
   *  @param that the manifest to compare against
   *  @return `true` if `that` is this manifest, `Manifest.Any`, or `Manifest.AnyVal`,
   *          `false` otherwise
   */
  override def <:<(that: ClassManifest[_]): Boolean =
    (that eq this) || (that eq Manifest.Any) || (that eq Manifest.AnyVal)
  /** Tests whether `other` can possibly equal this manifest: only `AnyValManifest`
   *  instances can.
   *
   *  @param other the value to test
   */
  override def canEqual(other: Any) = other match {
    case _: AnyValManifest[_] => true
    case _                    => false
  }
  /** Tests whether `that` is the same instance as this manifest; equality of value
   *  manifests is reference identity.
   *
   *  @param that the value to compare against
   *  @return `true` if `that` is this exact instance, `false` otherwise
   */
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  /** Returns the identity hash code of this manifest, consistent with its
   *  reference-identity `equals`.
   */
  override def hashCode = System.identityHashCode(this)
}

/** `ManifestFactory` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 *
 *  Unlike `Manifest`, this factory isn't annotated with a deprecation warning.
 *  This is done to prevent avalanches of deprecation warnings in the code that calls methods with manifests.
 *  Why so complicated? Read up the comments for `ClassManifestFactory`.
 */
object ManifestFactory {
  /** Returns the manifests for the value types `Byte`, `Short`, `Char`, `Int`,
   *  `Long`, `Float`, `Double`, `Boolean`, and `Unit`, in that order.
   */
  def valueManifests: List[AnyValManifest[_]] =
    List(Byte, Short, Char, Int, Long, Float, Double, Boolean, Unit)

  @SerialVersionUID(1L)
  private[reflect] class ByteManifest extends AnyValManifest[scala.Byte]("Byte") {
    /** Returns `java.lang.Byte.TYPE`, the `Class` object of the primitive `byte` type. */
    def runtimeClass = java.lang.Byte.TYPE
    /** Returns a new `Array[Byte]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Byte] = new Array[Byte](len)
    /** Returns a new `ArraySeq.ofByte` wrapping a new `Array[Byte]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Byte] = new ArraySeq.ofByte(new Array[Byte](len))
    /** Returns a new `ArrayBuilder.ofByte` for building an `Array[Byte]`. */
    override def newArrayBuilder(): ArrayBuilder[Byte] = new ArrayBuilder.ofByte()
    /** Extractor that matches values of type `Byte`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is a `Byte`, `None` otherwise
     */
    override def unapply(x: Any): Option[Byte] = {
      x match {
        case d: Byte => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Byte
  }
  private object ByteManifest extends ByteManifest
  /** The manifest for the value type `Byte`, whose `runtimeClass` is `java.lang.Byte.TYPE`. */
  val Byte: ByteManifest = ByteManifest

  @SerialVersionUID(1L)
  private[reflect] class ShortManifest extends AnyValManifest[scala.Short]("Short") {
    /** Returns `java.lang.Short.TYPE`, the `Class` object of the primitive `short` type. */
    def runtimeClass = java.lang.Short.TYPE
    /** Returns a new `Array[Short]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Short] = new Array[Short](len)
    /** Returns a new `ArraySeq.ofShort` wrapping a new `Array[Short]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Short] = new ArraySeq.ofShort(new Array[Short](len))
    /** Returns a new `ArrayBuilder.ofShort` for building an `Array[Short]`. */
    override def newArrayBuilder(): ArrayBuilder[Short] = new ArrayBuilder.ofShort()
    /** Extractor that matches values of type `Short`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is a `Short`, `None` otherwise
     */
    override def unapply(x: Any): Option[Short] = {
      x match {
        case d: Short => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Short
  }
  private object ShortManifest extends ShortManifest
  /** The manifest for the value type `Short`, whose `runtimeClass` is `java.lang.Short.TYPE`. */
  val Short: ShortManifest = ShortManifest

  @SerialVersionUID(1L)
  private[reflect] class CharManifest extends AnyValManifest[scala.Char]("Char") {
    /** Returns `java.lang.Character.TYPE`, the `Class` object of the primitive `char` type. */
    def runtimeClass = java.lang.Character.TYPE
    /** Returns a new `Array[Char]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Char] = new Array[Char](len)
    /** Returns a new `ArraySeq.ofChar` wrapping a new `Array[Char]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Char] = new ArraySeq.ofChar(new Array[Char](len))
    /** Returns a new `ArrayBuilder.ofChar` for building an `Array[Char]`. */
    override def newArrayBuilder(): ArrayBuilder[Char] = new ArrayBuilder.ofChar()
    /** Extractor that matches values of type `Char`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is a `Char`, `None` otherwise
     */
    override def unapply(x: Any): Option[Char] = {
      x match {
        case d: Char => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Char
  }
  private object CharManifest extends CharManifest
  /** The manifest for the value type `Char`, whose `runtimeClass` is `java.lang.Character.TYPE`. */
  val Char: CharManifest = CharManifest

  @SerialVersionUID(1L)
  private[reflect] class IntManifest extends AnyValManifest[scala.Int]("Int") {
    /** Returns `java.lang.Integer.TYPE`, the `Class` object of the primitive `int` type. */
    def runtimeClass = java.lang.Integer.TYPE
    /** Returns a new `Array[Int]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Int] = new Array[Int](len)
    /** Returns a new `ArraySeq.ofInt` wrapping a new `Array[Int]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Int] = new ArraySeq.ofInt(new Array[Int](len))
    /** Returns a new `ArrayBuilder.ofInt` for building an `Array[Int]`. */
    override def newArrayBuilder(): ArrayBuilder[Int] = new ArrayBuilder.ofInt()
    /** Extractor that matches values of type `Int`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is an `Int`, `None` otherwise
     */
    override def unapply(x: Any): Option[Int] = {
      x match {
        case d: Int => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Int
  }
  private object IntManifest extends IntManifest
  /** The manifest for the value type `Int`, whose `runtimeClass` is `java.lang.Integer.TYPE`. */
  val Int: IntManifest = IntManifest

  @SerialVersionUID(1L)
  private[reflect] class LongManifest extends AnyValManifest[scala.Long]("Long") {
    /** Returns `java.lang.Long.TYPE`, the `Class` object of the primitive `long` type. */
    def runtimeClass = java.lang.Long.TYPE
    /** Returns a new `Array[Long]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Long] = new Array[Long](len)
    /** Returns a new `ArraySeq.ofLong` wrapping a new `Array[Long]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Long] = new ArraySeq.ofLong(new Array[Long](len))
    /** Returns a new `ArrayBuilder.ofLong` for building an `Array[Long]`. */
    override def newArrayBuilder(): ArrayBuilder[Long] = new ArrayBuilder.ofLong()
    /** Extractor that matches values of type `Long`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is a `Long`, `None` otherwise
     */
    override def unapply(x: Any): Option[Long] = {
      x match {
        case d: Long => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Long
  }
  private object LongManifest extends LongManifest
  /** The manifest for the value type `Long`, whose `runtimeClass` is `java.lang.Long.TYPE`. */
  val Long: LongManifest = LongManifest

  @SerialVersionUID(1L)
  private[reflect] class FloatManifest extends AnyValManifest[scala.Float]("Float") {
    /** Returns `java.lang.Float.TYPE`, the `Class` object of the primitive `float` type. */
    def runtimeClass = java.lang.Float.TYPE
    /** Returns a new `Array[Float]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Float] = new Array[Float](len)
    /** Returns a new `ArraySeq.ofFloat` wrapping a new `Array[Float]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Float] = new ArraySeq.ofFloat(new Array[Float](len))
    /** Returns a new `ArrayBuilder.ofFloat` for building an `Array[Float]`. */
    override def newArrayBuilder(): ArrayBuilder[Float] = new ArrayBuilder.ofFloat()
    /** Extractor that matches values of type `Float`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is a `Float`, `None` otherwise
     */
    override def unapply(x: Any): Option[Float] = {
      x match {
        case d: Float => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Float
  }
  private object FloatManifest extends FloatManifest
  /** The manifest for the value type `Float`, whose `runtimeClass` is `java.lang.Float.TYPE`. */
  val Float: FloatManifest = FloatManifest

  @SerialVersionUID(1L)
  private[reflect] class DoubleManifest extends AnyValManifest[scala.Double]("Double") {
    /** Returns `java.lang.Double.TYPE`, the `Class` object of the primitive `double` type. */
    def runtimeClass = java.lang.Double.TYPE
    /** Returns a new `Array[Double]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Double] = new Array[Double](len)
    /** Returns a new `ArraySeq.ofDouble` wrapping a new `Array[Double]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Double] = new ArraySeq.ofDouble(new Array[Double](len))
    /** Returns a new `ArrayBuilder.ofDouble` for building an `Array[Double]`. */
    override def newArrayBuilder(): ArrayBuilder[Double] = new ArrayBuilder.ofDouble()

    /** Extractor that matches values of type `Double`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is a `Double`, `None` otherwise
     */
    override def unapply(x: Any): Option[Double] = {
      x match {
        case d: Double => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Double
  }
  private object DoubleManifest extends DoubleManifest
  /** The manifest for the value type `Double`, whose `runtimeClass` is `java.lang.Double.TYPE`. */
  val Double: DoubleManifest = DoubleManifest

  @SerialVersionUID(1L)
  private[reflect] class BooleanManifest extends AnyValManifest[scala.Boolean]("Boolean") {
    /** Returns `java.lang.Boolean.TYPE`, the `Class` object of the primitive `boolean` type. */
    def runtimeClass = java.lang.Boolean.TYPE
    /** Returns a new `Array[Boolean]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Boolean] = new Array[Boolean](len)
    /** Returns a new `ArraySeq.ofBoolean` wrapping a new `Array[Boolean]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Boolean] = new ArraySeq.ofBoolean(new Array[Boolean](len))
    /** Returns a new `ArrayBuilder.ofBoolean` for building an `Array[Boolean]`. */
    override def newArrayBuilder(): ArrayBuilder[Boolean] = new ArrayBuilder.ofBoolean()
    /** Extractor that matches values of type `Boolean`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is a `Boolean`, `None` otherwise
     */
    override def unapply(x: Any): Option[Boolean] = {
      x match {
        case d: Boolean => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Boolean
  }
  private object BooleanManifest extends BooleanManifest
  /** The manifest for the value type `Boolean`, whose `runtimeClass` is `java.lang.Boolean.TYPE`. */
  val Boolean: BooleanManifest = BooleanManifest

  @SerialVersionUID(1L)
  private[reflect] class UnitManifest extends AnyValManifest[scala.Unit]("Unit") {
    /** Returns `java.lang.Void.TYPE`, the `Class` object of the `void` type. */
    def runtimeClass = java.lang.Void.TYPE
    /** Returns a new `Array[Unit]` of the given length.
     *
     *  @param len the length of the new array
     */
    @inline override def newArray(len: Int): Array[Unit] = new Array[Unit](len)
    /** Returns a new `ArraySeq.ofUnit` wrapping a new `Array[Unit]` of the given length.
     *
     *  @param len the length of the underlying array
     */
    override def newWrappedArray(len: Int): ArraySeq[Unit] = new ArraySeq.ofUnit(new Array[Unit](len))
    /** Returns a new `ArrayBuilder.ofUnit` for building an `Array[Unit]`. */
    override def newArrayBuilder(): ArrayBuilder[Unit] = new ArrayBuilder.ofUnit()
    /** Returns the `Class` of arrays with element class `tp`. Arrays of `Unit` are
     *  represented as arrays of `scala.runtime.BoxedUnit` at runtime.
     *
     *  @tparam T the element type of the array class
     *  @param tp the element class
     *  @return `classOf[Array[scala.runtime.BoxedUnit]]` if `tp` is `runtimeClass`,
     *          otherwise the array class computed by the inherited implementation
     */
    override protected def arrayClass[T](tp: Class[_]): Class[Array[T]] =
      if (tp eq runtimeClass) classOf[Array[scala.runtime.BoxedUnit]].asInstanceOf[Class[Array[T]]]
      else super.arrayClass(tp)
    /** Extractor that matches values of type `Unit`.
     *
     *  @param x the value to match
     *  @return `Some(x)` if `x` is a `Unit`, `None` otherwise
     */
    override def unapply(x: Any): Option[Unit] = {
      x match {
        case d: Unit => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Unit
  }
  private object UnitManifest extends UnitManifest
  /** The manifest for the value type `Unit`, whose `runtimeClass` is `java.lang.Void.TYPE`. */
  val Unit: UnitManifest = UnitManifest

  private object AnyManifest extends PhantomManifest[scala.Any](classOf[java.lang.Object], "Any") {
    /** Returns `classOf[java.lang.Object]`, the erasure of `Any`. */
    override def runtimeClass = classOf[java.lang.Object]
    /** Returns a new `Array[scala.Any]` of the given length.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Any](len)
    /** Tests whether the type represented by this manifest is a subtype of the type
     *  represented by `that`.
     *
     *  @param that the manifest to compare against
     *  @return `true` only if `that` is this manifest itself, `false` otherwise
     */
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this)
    private def readResolve(): Any = Manifest.Any
  }
  /** The manifest for the type `Any`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val Any: Manifest[scala.Any] = AnyManifest

  private object ObjectManifest extends PhantomManifest[java.lang.Object](classOf[java.lang.Object], "Object") {
    /** Returns `classOf[java.lang.Object]`. */
    override def runtimeClass = classOf[java.lang.Object]
    /** Returns a new `Array[java.lang.Object]` of the given length.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[java.lang.Object](len)
    /** Tests whether the type represented by this manifest is a subtype of the type
     *  represented by `that`.
     *
     *  @param that the manifest to compare against
     *  @return `true` if `that` is this manifest or the manifest for `Any`, `false` otherwise
     */
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.Object
  }
  /** The manifest for the type `Object`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val Object: Manifest[java.lang.Object] = ObjectManifest

  /** The manifest for the type `AnyRef`, the same instance as `Object`. */
  val AnyRef: Manifest[scala.AnyRef] = Object

  private object AnyValManifest extends PhantomManifest[scala.AnyVal](classOf[java.lang.Object], "AnyVal") {
    /** Returns `classOf[java.lang.Object]`, the erasure of `AnyVal`. */
    override def runtimeClass = classOf[java.lang.Object]
    /** Returns a new `Array[scala.AnyVal]` of the given length.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.AnyVal](len)
    /** Tests whether the type represented by this manifest is a subtype of the type
     *  represented by `that`.
     *
     *  @param that the manifest to compare against
     *  @return `true` if `that` is this manifest or the manifest for `Any`, `false` otherwise
     */
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.AnyVal
  }
  /** The manifest for the type `AnyVal`, whose `runtimeClass` is `classOf[java.lang.Object]`. */
  val AnyVal: Manifest[scala.AnyVal] = AnyValManifest

  private object NullManifest extends PhantomManifest[scala.Null](classOf[scala.runtime.Null$], "Null") {
    /** Returns `classOf[scala.runtime.Null$]`, the class used to represent `Null` at runtime. */
    override def runtimeClass = classOf[scala.runtime.Null$]
    /** Returns a new `Array[scala.Null]` of the given length.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Null](len)
    /** Tests whether the type represented by this manifest is a subtype of the type
     *  represented by `that`.
     *
     *  @param that the manifest to compare against
     *  @return `true` if `that` is non-null, is not the manifest for `Nothing`, and does
     *          not represent a subtype of `AnyVal`, `false` otherwise
     */
    override def <:<(that: ClassManifest[_]): Boolean =
      (that ne null) && (that ne Nothing) && !(that <:< AnyVal)
    private def readResolve(): Any = Manifest.Null
  }
  /** The manifest for the type `Null`, whose `runtimeClass` is `classOf[scala.runtime.Null$]`. */
  val Null: Manifest[scala.Null] = NullManifest

  private object NothingManifest extends PhantomManifest[scala.Nothing](classOf[scala.runtime.Nothing$], "Nothing") {
    /** Returns `classOf[scala.runtime.Nothing$]`, the class used to represent `Nothing` at runtime. */
    override def runtimeClass = classOf[scala.runtime.Nothing$]
    /** Returns a new `Array[scala.Nothing]` of the given length.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Nothing](len)
    /** Tests whether the type represented by this manifest is a subtype of the type
     *  represented by `that`.
     *
     *  @param that the manifest to compare against
     *  @return `true` for every non-null `that`, since `Nothing` is a subtype of every type
     */
    override def <:<(that: ClassManifest[_]): Boolean = (that ne null)
    private def readResolve(): Any = Manifest.Nothing
  }
  /** The manifest for the type `Nothing`, whose `runtimeClass` is `classOf[scala.runtime.Nothing$]`. */
  val Nothing: Manifest[scala.Nothing] = NothingManifest

  @SerialVersionUID(1L)
  private class SingletonTypeManifest[T <: AnyRef](value: AnyRef) extends Manifest[T] {
    /** The runtime class of `value`, computed on first access. */
    lazy val runtimeClass = value.getClass
    /** The string representation of `value` followed by `".type"`, computed on first access. */
    override lazy val toString = value.toString + ".type"
  }

  /** Manifest for the singleton type `value.type`.
   *
   *  @tparam T the type to manifest, constrained to `AnyRef` subtypes
   *  @param value the singleton instance for which to create a manifest
   */
  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] =
    new SingletonTypeManifest[T](value)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a top-level or static class.
   *  @note This no-prefix, no-arguments case is separate because we
   *       it's called from ScalaRunTime.boxArray itself. If we
   *       pass varargs as arrays into this, we get an infinitely recursive call
   *       to boxArray. (Besides, having a separate case is more efficient)
   *
   *  @tparam T the type to manifest
   *  @param clazz the runtime `Class` representing the erased type `T`
   */
  def classType[T](clazz: Predef.Class[_]): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** Manifest for the class type `clazz`, where `clazz` is
   *  a top-level or static class and args are its type arguments. 
   *
   *  @tparam T the type to manifest
   *  @param clazz the runtime `Class` representing the erased type `T`
   *  @param arg1 the manifest for the first type argument
   *  @param args the manifests for the remaining type arguments
   */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[_], args: Manifest[_]*): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a class with non-package prefix type `prefix` and type arguments `args`.
   *
   *  @tparam T the type to manifest
   *  @param prefix the manifest for the enclosing type (non-package prefix)
   *  @param clazz the runtime `Class` representing the erased type `T`
   *  @param args the manifests for the type arguments
   */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  @SerialVersionUID(1L)
  private abstract class PhantomManifest[T](_runtimeClass: Predef.Class[_],
                                            /** The name of the represented type, used as the string representation of this manifest */
                                            override val toString: String) extends ClassTypeManifest[T](None, _runtimeClass, Nil) {
    /** Tests whether `that` is the same instance as this manifest; equality of
     *  phantom manifests is reference identity.
     *
     *  @param that the value to compare against
     *  @return `true` if `that` is this exact instance, `false` otherwise
     */
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    /** Returns the identity hash code of this manifest, consistent with its
     *  reference-identity `equals`.
     */
    override def hashCode = System.identityHashCode(this)
  }

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a top-level or static class. 
   */
  @SerialVersionUID(1L)
  private class ClassTypeManifest[T](prefix: Option[Manifest[_]],
                                     runtimeClass1: Predef.Class[_],
                                     /** The manifests for the type arguments of this class type */
                                     override val typeArguments: List[Manifest[_]]) extends Manifest[T] {
    /** Returns the runtime class supplied at construction, the erasure of `T`. */
    def runtimeClass: Predef.Class[_] = runtimeClass1
    /** Returns the string representation of this class type: the prefix followed by
     *  `"#"`, if any, then the class name (`"Array"` for array classes), then the type
     *  arguments in square brackets, if any.
     */
    override def toString =
      (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
      (if (runtimeClass.isArray) "Array" else runtimeClass.getName) +
      argString
   }

  /** Manifest for the array type `Array[T]`, where `arg` manifests the element type `T`.
   *
   *  @tparam T the element type of the array
   *  @param arg the manifest for the element type `T`
   *  @return the array manifest of `arg`
   */
  def arrayType[T](arg: Manifest[_]): Manifest[Array[T]] =
    arg.asInstanceOf[Manifest[T]].arrayManifest

  @SerialVersionUID(1L)
  private class AbstractTypeManifest[T](prefix: Manifest[_], name: String, upperBound: Predef.Class[_], args: scala.collection.Seq[Manifest[_]]) extends Manifest[T] {
    /** Returns `upperBound`, the erasure of the abstract type. */
    def runtimeClass = upperBound
    /** The manifests for the type arguments, as a list. */
    override val typeArguments = args.toList
    /** Returns the prefix, `"#"`, the name of the abstract type, and the type
     *  arguments in square brackets, if any.
     */
    override def toString = prefix.toString+"#"+name+argString
  }

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
   *  strictly necessary as it could be obtained by reflection. It was
   *  added so that erasure can be calculated without reflection. 
   *
   *  @tparam T the type to manifest
   *  @param prefix the manifest for the type in which this abstract type is a member
   *  @param name the name of the abstract type member
   *  @param upperBound the `Class` representing the erasure of the abstract type's upper bound
   *  @param args the manifests for the type arguments
   */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new AbstractTypeManifest[T](prefix, name, upperBound, args)

  @SerialVersionUID(1L)
  private class WildcardManifest[T](lowerBound: Manifest[_], upperBound: Manifest[_]) extends Manifest[T] {
    /** Returns the runtime class of the upper bound. */
    def runtimeClass = upperBound.runtimeClass
    /** Returns `"_"`, followed by `" >: "` and the lower bound unless it is `Nothing`,
     *  then `" <: "` and the upper bound unless it is `Nothing`.
     */
    override def toString =
      "_" +
        (if (lowerBound eq Nothing) "" else " >: "+lowerBound) +
        (if (upperBound eq Nothing) "" else " <: "+upperBound)
  }

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
   *
   *  @tparam T the type to manifest
   *  @param lowerBound the manifest for the wildcard's lower bound `L`
   *  @param upperBound the manifest for the wildcard's upper bound `U`
   */
  def wildcardType[T](lowerBound: Manifest[_], upperBound: Manifest[_]): Manifest[T] =
    new WildcardManifest[T](lowerBound, upperBound)

  @SerialVersionUID(1L)
  private class IntersectionTypeManifest[T](parents: Array[Manifest[_]]) extends Manifest[T] {
    // We use an `Array` instead of a `Seq` for `parents` to avoid cyclic dependencies during deserialization
    // which can cause serialization proxies to leak and cause a ClassCastException.
    /** Returns the runtime class of the first parent in the intersection. */
    def runtimeClass = parents(0).runtimeClass
    /** Returns the parent manifests joined by `" with "`. */
    override def toString = parents.mkString(" with ")
  }

  /** Manifest for the intersection type `parents_0 with ... with parents_n`.
   *
   *  @tparam T the type to manifest
   *  @param parents the manifests for each type in the intersection
   */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    new IntersectionTypeManifest[T](parents.toArray)
}
