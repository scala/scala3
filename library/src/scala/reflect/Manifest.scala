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
import scala.annotation.{implicitNotFound, nowarn}
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
 *  ```scala sc:compile
 *    import scala.reflect.Manifest
 *
 *    def arr[T: Manifest]: Array[T] = new Array[T](0)
 *
 *    // Methods manifest and optManifest are in [[scala.Predef]].
 *    def isApproxSubType[T: Manifest, U: Manifest]: Boolean = manifest[T] <:< manifest[U]
 *    val stringsAreAnyRefs = isApproxSubType[List[String], List[AnyRef]]
 *    val stringsAreInts = isApproxSubType[List[String], List[Int]]
 *
 *    def methods[T: Manifest] = manifest[T].runtimeClass.getMethods
 *    def retType[T: Manifest](name: String) =
 *      methods[T].find(_.getName == name).map(_.getGenericReturnType)
 *
 *    val mapValuesReturnType = retType[Map[_, _]]("values")
 *  ```
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
@implicitNotFound(msg = "No Manifest available for ${T}.")
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use scala.reflect.ClassTag (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
trait Manifest[T] extends ClassManifest[T] with Equals {
  /** Returns the manifests for the type arguments of the represented type, or `Nil` if there are none. */
  override def typeArguments: List[Manifest[?]] = Nil

  /** Returns a manifest for the array type `Array[T]`. */
  override def arrayManifest: Manifest[Array[T]] =
    Manifest.classType[Array[T]](arrayClass[T](runtimeClass), this)

  /** Returns `true` if `that` is a `Manifest`, and so is a candidate for equality with this manifest.
   *
   *  @param that the value to test for comparability with this manifest
   */
  override def canEqual(that: Any): Boolean = that match {
    case _: Manifest[?]   => true
    case _                => false
  }
  /** Note: testing for erasure here is important, as it is many times
   *  faster than <:< and rules out most comparisons.
   *
   *  @param that the object to compare for equality (only `Manifest` instances can be equal)
   */
  override def equals(that: Any): Boolean = that match {
    case m: Manifest[?] => (m canEqual this) && (this.runtimeClass == m.runtimeClass) && (this <:< m) && (m <:< this)
    case _              => false
  }
  /** Returns a hash code derived from `runtimeClass`, so that manifests with the same erasure hash alike. */
  override def hashCode() = this.runtimeClass.##
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

  /** Returns the manifests for the nine value types, in the order `Byte`, `Short`, `Char`, `Int`,
   *  `Long`, `Float`, `Double`, `Boolean`, `Unit`.
   */
  def valueManifests: List[AnyValManifest[?]] =
    ManifestFactory.valueManifests

  val Byte: ManifestFactory.ByteManifest = ManifestFactory.Byte
  val Short: ManifestFactory.ShortManifest = ManifestFactory.Short
  val Char: ManifestFactory.CharManifest = ManifestFactory.Char
  val Int: ManifestFactory.IntManifest = ManifestFactory.Int
  val Long: ManifestFactory.LongManifest = ManifestFactory.Long
  val Float: ManifestFactory.FloatManifest = ManifestFactory.Float
  val Double: ManifestFactory.DoubleManifest = ManifestFactory.Double
  val Boolean: ManifestFactory.BooleanManifest = ManifestFactory.Boolean
  val Unit: ManifestFactory.UnitManifest = ManifestFactory.Unit

  val Any: Manifest[scala.Any] = ManifestFactory.Any
  val Object: Manifest[java.lang.Object] = ManifestFactory.Object
  val AnyRef: Manifest[scala.AnyRef] = ManifestFactory.AnyRef
  val AnyVal: Manifest[scala.AnyVal] = ManifestFactory.AnyVal
  val Null: Manifest[scala.Null] = ManifestFactory.Null
  val Nothing: Manifest[scala.Nothing] = ManifestFactory.Nothing

  /** Manifest for the singleton type `value.type`.
   *
   *  @tparam T the type to be represented, typically inferred as the singleton type of `value`
   *  @param value the runtime object whose singleton type is represented
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
   *  @tparam T the type represented by this manifest
   *  @param clazz the runtime `Class` for the type `T`
   */
  def classType[T](clazz: Predef.Class[?]): Manifest[T] =
    ManifestFactory.classType[T](clazz)

  /** Manifest for the class type `clazz`, where `clazz` is
   *  a top-level or static class and args are its type arguments.
   *
   *  @tparam T the type represented by this manifest
   *  @param clazz the runtime `Class` for the type `T`
   *  @param arg1 the manifest for the first type argument (required to ensure at least one type argument)
   *  @param args the manifests for the remaining type arguments
   */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[?], args: Manifest[?]*): Manifest[T] =
    ManifestFactory.classType[T](clazz, arg1, args*)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a class with non-package prefix type `prefix` and type arguments `args`.
   *
   *  @tparam T the type represented by this manifest
   *  @param prefix the manifest for the non-package prefix type
   *  @param clazz the runtime `Class` for the type `T`
   *  @param args the manifests for the type arguments
   */
  def classType[T](prefix: Manifest[?], clazz: Predef.Class[?], args: Manifest[?]*): Manifest[T] =
    ManifestFactory.classType[T](prefix, clazz, args*)

  /** Returns the manifest for the array type `Array[T]`, given the manifest `arg` for the element type `T`.
   *
   *  @tparam T the element type of the array type described by the result
   *  @param arg the manifest for the element type
   *  @return the `arrayManifest` obtained by casting `arg` to `Manifest[T]`; the cast is unchecked,
   *          so `arg` is assumed to describe `T`
   */
  def arrayType[T](arg: Manifest[?]): Manifest[Array[T]] =
    ManifestFactory.arrayType[T](arg)

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
   *  strictly necessary as it could be obtained by reflection. It was
   *  added so that erasure can be calculated without reflection.
   *
   *  @tparam T the type represented by this manifest
   *  @param prefix the manifest for the type containing this abstract type member
   *  @param name the name of the abstract type member
   *  @param upperBound the runtime `Class` of the upper bound, used to compute erasure without reflection
   *  @param args the manifests for the type arguments
   */
  def abstractType[T](prefix: Manifest[?], name: String, upperBound: Predef.Class[?], args: Manifest[?]*): Manifest[T] =
    ManifestFactory.abstractType[T](prefix, name, upperBound, args*)

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
   *
   *  @tparam T the type represented by this manifest
   *  @param lowerBound the manifest for the lower bound `L` of the wildcard
   *  @param upperBound the manifest for the upper bound `U` of the wildcard
   */
  def wildcardType[T](lowerBound: Manifest[?], upperBound: Manifest[?]): Manifest[T] =
    ManifestFactory.wildcardType[T](lowerBound, upperBound)

  /** Manifest for the intersection type `parents_0 with ... with parents_n`.
   *
   *  @tparam T the type represented by this manifest
   *  @param parents the manifests for each type in the intersection
   */
  def intersectionType[T](parents: Manifest[?]*): Manifest[T] =
    ManifestFactory.intersectionType[T](parents*)

}

/** A `Manifest` for one of the value types, such as `Int` or `Boolean`.
 *
 *  Instances of this class are compared by reference identity, and the represented type conforms
 *  only to itself, `Any` and `AnyVal`. The canonical manifest for each value type is the single
 *  instance supplied by `Manifest`, such as `Manifest.Int`.
 *
 *  @tparam T the value type described by this manifest
 *  @param toString the name of the value type, such as `"Int"`, used as the string representation
 *                  of this manifest
 */
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use type tags and manually check the corresponding class or type instead", "2.10.0")
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
@SerialVersionUID(1L)
abstract class AnyValManifest[T <: AnyVal](override val toString: String) extends Manifest[T] with Equals {
  override def <:<(that: ClassManifest[?]): Boolean =
    (that eq this) || (that eq Manifest.Any) || (that eq Manifest.AnyVal)
  /** Returns `true` if `other` is an `AnyValManifest`, and so is a candidate for equality with this manifest.
   *
   *  @param other the value to test for comparability with this manifest
   */
  override def canEqual(other: Any) = other match {
    case _: AnyValManifest[?] => true
    case _                    => false
  }
  /** Returns `true` only if `that` is this very manifest, since equality for value type manifests is
   *  reference identity.
   *
   *  @param that the value to compare with this manifest
   */
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  /** Returns the identity hash code of this manifest, consistent with its reference-identity `equals`. */
  override def hashCode = System.identityHashCode(this)
}

/** `ManifestFactory` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 *
 *  Unlike `Manifest`, this factory isn't annotated with a deprecation warning.
 *  This is done to prevent avalanches of deprecation warnings in the code that calls methods with manifests.
 *  Why so complicated? Read up the comments for `ClassManifestFactory`.
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
object ManifestFactory {
  /** Returns the manifests for the nine value types, in the order `Byte`, `Short`, `Char`, `Int`,
   *  `Long`, `Float`, `Double`, `Boolean`, `Unit`.
   */
  def valueManifests: List[AnyValManifest[?]] =
    List(Byte, Short, Char, Int, Long, Float, Double, Boolean, Unit)

  @SerialVersionUID(1L)
  final private[reflect] class ByteManifest extends AnyValManifest[scala.Byte]("Byte") {
    /** Returns the `Class` for the primitive type `byte`. */
    def runtimeClass: Class[java.lang.Byte] = java.lang.Byte.TYPE
    @inline override def newArray(len: Int): Array[Byte] = new Array[Byte](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Byte]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofByte` wrapping an `Array[Byte]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Byte] = new ArraySeq.ofByte(new Array[Byte](len))
    /** Returns a new builder for arrays with element type `Byte`. */
    override def newArrayBuilder(): ArrayBuilder[Byte] = new ArrayBuilder.ofByte()
    /** Matches `x` only if it is a `Byte`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being a `Byte`
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
  val Byte: ByteManifest = new ByteManifest

  @SerialVersionUID(1L)
  final private[reflect] class ShortManifest extends AnyValManifest[scala.Short]("Short") {
    /** Returns the `Class` for the primitive type `short`. */
    def runtimeClass: Class[java.lang.Short] = java.lang.Short.TYPE
    @inline override def newArray(len: Int): Array[Short] = new Array[Short](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Short]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofShort` wrapping an `Array[Short]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Short] = new ArraySeq.ofShort(new Array[Short](len))
    /** Returns a new builder for arrays with element type `Short`. */
    override def newArrayBuilder(): ArrayBuilder[Short] = new ArrayBuilder.ofShort()
    /** Matches `x` only if it is a `Short`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being a `Short`
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
  val Short: ShortManifest = new ShortManifest

  @SerialVersionUID(1L)
  final private[reflect] class CharManifest extends AnyValManifest[scala.Char]("Char") {
    /** Returns the `Class` for the primitive type `char`. */
    def runtimeClass: Class[java.lang.Character] = java.lang.Character.TYPE
    @inline override def newArray(len: Int): Array[Char] = new Array[Char](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Char]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofChar` wrapping an `Array[Char]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Char] = new ArraySeq.ofChar(new Array[Char](len))
    /** Returns a new builder for arrays with element type `Char`. */
    override def newArrayBuilder(): ArrayBuilder[Char] = new ArrayBuilder.ofChar()
    /** Matches `x` only if it is a `Char`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being a `Char`
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
  val Char: CharManifest = new CharManifest

  @SerialVersionUID(1L)
  final private[reflect] class IntManifest extends AnyValManifest[scala.Int]("Int") {
    /** Returns the `Class` for the primitive type `int`. */
    def runtimeClass: Class[java.lang.Integer] = java.lang.Integer.TYPE
    @inline override def newArray(len: Int): Array[Int] = new Array[Int](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Int]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofInt` wrapping an `Array[Int]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Int] = new ArraySeq.ofInt(new Array[Int](len))
    /** Returns a new builder for arrays with element type `Int`. */
    override def newArrayBuilder(): ArrayBuilder[Int] = new ArrayBuilder.ofInt()
    /** Matches `x` only if it is an `Int`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being an `Int`
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
  val Int: IntManifest = new IntManifest

  @SerialVersionUID(1L)
  final private[reflect] class LongManifest extends AnyValManifest[scala.Long]("Long") {
    /** Returns the `Class` for the primitive type `long`. */
    def runtimeClass: Class[java.lang.Long] = java.lang.Long.TYPE
    @inline override def newArray(len: Int): Array[Long] = new Array[Long](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Long]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofLong` wrapping an `Array[Long]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Long] = new ArraySeq.ofLong(new Array[Long](len))
    /** Returns a new builder for arrays with element type `Long`. */
    override def newArrayBuilder(): ArrayBuilder[Long] = new ArrayBuilder.ofLong()
    /** Matches `x` only if it is a `Long`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being a `Long`
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
  val Long: LongManifest = new LongManifest

  @SerialVersionUID(1L)
  final private[reflect] class FloatManifest extends AnyValManifest[scala.Float]("Float") {
    /** Returns the `Class` for the primitive type `float`. */
    def runtimeClass: Class[java.lang.Float] = java.lang.Float.TYPE
    @inline override def newArray(len: Int): Array[Float] = new Array[Float](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Float]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofFloat` wrapping an `Array[Float]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Float] = new ArraySeq.ofFloat(new Array[Float](len))
    /** Returns a new builder for arrays with element type `Float`. */
    override def newArrayBuilder(): ArrayBuilder[Float] = new ArrayBuilder.ofFloat()
    /** Matches `x` only if it is a `Float`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being a `Float`
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
  val Float: FloatManifest = new FloatManifest

  @SerialVersionUID(1L)
  final private[reflect] class DoubleManifest extends AnyValManifest[scala.Double]("Double") {
    /** Returns the `Class` for the primitive type `double`. */
    def runtimeClass: Class[java.lang.Double] = java.lang.Double.TYPE
    @inline override def newArray(len: Int): Array[Double] = new Array[Double](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Double]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofDouble` wrapping an `Array[Double]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Double] = new ArraySeq.ofDouble(new Array[Double](len))
    /** Returns a new builder for arrays with element type `Double`. */
    override def newArrayBuilder(): ArrayBuilder[Double] = new ArrayBuilder.ofDouble()

    /** Matches `x` only if it is a `Double`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being a `Double`
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
  val Double: DoubleManifest = new DoubleManifest

  @SerialVersionUID(1L)
  final private[reflect] class BooleanManifest extends AnyValManifest[scala.Boolean]("Boolean") {
    /** Returns the `Class` for the primitive type `boolean`. */
    def runtimeClass: Class[java.lang.Boolean] = java.lang.Boolean.TYPE
    @inline override def newArray(len: Int): Array[Boolean] = new Array[Boolean](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Boolean]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofBoolean` wrapping an `Array[Boolean]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Boolean] = new ArraySeq.ofBoolean(new Array[Boolean](len))
    /** Returns a new builder for arrays with element type `Boolean`. */
    override def newArrayBuilder(): ArrayBuilder[Boolean] = new ArrayBuilder.ofBoolean()
    /** Matches `x` only if it is a `Boolean`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being a `Boolean`
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
  val Boolean: BooleanManifest = new BooleanManifest

  @SerialVersionUID(1L)
  final private[reflect] class UnitManifest extends AnyValManifest[scala.Unit]("Unit") {
    /** Returns the `Class` for the primitive type `void`. */
    def runtimeClass: Class[java.lang.Void] = java.lang.Void.TYPE
    @inline override def newArray(len: Int): Array[Unit] = new Array[Unit](len)
    /** Returns a new mutable sequence of length `len` backed by a freshly created `Array[Unit]`.
     *
     *  @param len the length of the underlying array
     *  @return a new `ArraySeq.ofUnit` wrapping an `Array[Unit]` of length `len`
     */
    override def newWrappedArray(len: Int): ArraySeq[Unit] = new ArraySeq.ofUnit(new Array[Unit](len))
    /** Returns a new builder for arrays with element type `Unit`. */
    override def newArrayBuilder(): ArrayBuilder[Unit] = new ArrayBuilder.ofUnit()
    /** Returns the `Class` object for arrays whose element class is `tp`, answering
     *  `Array[scala.runtime.BoxedUnit]` when `tp` is the `Unit` runtime class, since that is how
     *  arrays of `Unit` are represented at runtime.
     *
     *  @tparam T the element type of the array class
     *  @param tp the runtime `Class` of the array's element type
     *  @return the `Class` representing `Array[T]`. The cast to that type is unchecked, so `tp` is
     *          assumed to be the erasure of `T`.
     */
    override protected def arrayClass[T](tp: Class[?]): Class[Array[T]] =
      if (tp eq runtimeClass) classOf[Array[scala.runtime.BoxedUnit]].asInstanceOf[Class[Array[T]]]
      else super.arrayClass(tp)
    /** Matches `x` only if it is a `Unit`, so that this manifest can serve as an extractor.
     *
     *  @param x the value to test for being a `Unit`
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
  val Unit: UnitManifest = new UnitManifest

  private val ObjectTYPE = classOf[java.lang.Object]
  private val NothingTYPE = classOf[scala.runtime.Nothing$]
  private val NullTYPE = classOf[scala.runtime.Null$]

  @SerialVersionUID(1L)
  final private class AnyManifest extends PhantomManifest[scala.Any](ObjectTYPE, "Any") {
    /** Returns a new `Array[Any]` of length `len`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Any](len)
    override def <:<(that: ClassManifest[?]): Boolean = (that eq this)
    private def readResolve(): Any = Manifest.Any
  }
  val Any: Manifest[scala.Any] = new AnyManifest

  @SerialVersionUID(1L)
  final private class ObjectManifest extends PhantomManifest[java.lang.Object](ObjectTYPE, "Object") {
    /** Returns a new `Array[java.lang.Object]` of length `len`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[java.lang.Object](len)
    override def <:<(that: ClassManifest[?]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.Object
  }
  val Object: Manifest[java.lang.Object] = new ObjectManifest

  val AnyRef: Manifest[scala.AnyRef] = Object.asInstanceOf[Manifest[scala.AnyRef]]

  @SerialVersionUID(1L)
  final private class AnyValPhantomManifest extends PhantomManifest[scala.AnyVal](ObjectTYPE, "AnyVal") {
    /** Returns a new `Array[AnyVal]` of length `len`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.AnyVal](len)
    override def <:<(that: ClassManifest[?]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.AnyVal
  }
  val AnyVal: Manifest[scala.AnyVal] = new AnyValPhantomManifest

  @SerialVersionUID(1L)
  final private class NullManifest extends PhantomManifest[scala.Null](NullTYPE, "Null") {
    /** Returns a new `Array[Null]` of length `len`, all of whose elements are `null`.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Null](len)
    override def <:<(that: ClassManifest[?]): Boolean =
      (that ne null) && (that ne Nothing) && !(that <:< AnyVal)
    private def readResolve(): Any = Manifest.Null
  }
  val Null: Manifest[scala.Null] = new NullManifest

  @SerialVersionUID(1L)
  final private class NothingManifest extends PhantomManifest[scala.Nothing](NothingTYPE, "Nothing") {
    /** Returns a new `Array[Nothing]` of length `len`, all of whose elements are `null` since no
     *  value of type `Nothing` exists.
     *
     *  @param len the length of the new array
     */
    override def newArray(len: Int) = new Array[scala.Nothing](len)
    override def <:<(that: ClassManifest[?]): Boolean = (that ne null)
    private def readResolve(): Any = Manifest.Nothing
  }
  val Nothing: Manifest[scala.Nothing] = new NothingManifest

  @SerialVersionUID(1L)
  final private class SingletonTypeManifest[T <: AnyRef](value: AnyRef) extends Manifest[T] {
    lazy val runtimeClass: Class[? <: AnyRef] = value.getClass
    override lazy val toString = value.toString + ".type"
  }

  /** Manifest for the singleton type `value.type`.
   *
   *  @tparam T the type to be represented, typically inferred as the singleton type of `value`
   *  @param value the runtime object whose singleton type is represented
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
   *  @tparam T the type represented by this manifest
   *  @param clazz the runtime `Class` for the type `T`
   */
  def classType[T](clazz: Predef.Class[?]): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** Manifest for the class type `clazz`, where `clazz` is
   *  a top-level or static class and args are its type arguments.
   *
   *  @tparam T the type represented by this manifest
   *  @param clazz the runtime `Class` for the type `T`
   *  @param arg1 the manifest for the first type argument (required to ensure at least one type argument)
   *  @param args the manifests for the remaining type arguments
   */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[?], args: Manifest[?]*): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a class with non-package prefix type `prefix` and type arguments `args`.
   *
   *  @tparam T the type represented by this manifest
   *  @param prefix the manifest for the non-package prefix type
   *  @param clazz the runtime `Class` for the type `T`
   *  @param args the manifests for the type arguments
   */
  def classType[T](prefix: Manifest[?], clazz: Predef.Class[?], args: Manifest[?]*): Manifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  @SerialVersionUID(1L)
  private abstract class PhantomManifest[T](_runtimeClass: Predef.Class[?],
                                            override val toString: String) extends ClassTypeManifest[T](None, _runtimeClass, Nil) {
    /** Returns `true` only if `that` is this very manifest, since each phantom type is described by
     *  a single instance.
     *
     *  @param that the value to compare with this manifest
     */
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    /** Returns the identity hash code of this manifest, consistent with its reference-identity `equals`. */
    override def hashCode = System.identityHashCode(this)
  }

  /** Manifest for the class type `clazz[args]`, where `clazz` is
   *  a top-level or static class.
   */
  @SerialVersionUID(1L)
  private class ClassTypeManifest[T](prefix: Option[Manifest[?]],
                                     val runtimeClass: Predef.Class[?],
                                     override val typeArguments: List[Manifest[?]]) extends Manifest[T] {
    /** Returns the represented type rendered as three parts: the prefix followed by `#`, if there is
     *  a prefix; then `Array` if the runtime class is an array class, otherwise the runtime class
     *  name; then `argString`: the type arguments if there are any, otherwise the bracketed component
     *  type of the runtime class if it is an array class, and otherwise nothing.
     */
    override def toString() =
      (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
      (if (runtimeClass.isArray) "Array" else runtimeClass.getName) +
      argString
   }

  /** Returns the manifest for the array type `Array[T]`, given the manifest `arg` for the element type `T`.
   *
   *  @tparam T the element type of the array type described by the result
   *  @param arg the manifest for the element type
   *  @return the `arrayManifest` obtained by casting `arg` to `Manifest[T]`; the cast is unchecked,
   *          so `arg` is assumed to describe `T`
   */
  def arrayType[T](arg: Manifest[?]): Manifest[Array[T]] =
    arg.asInstanceOf[Manifest[T]].arrayManifest

  @SerialVersionUID(1L)
  private class AbstractTypeManifest[T](prefix: Manifest[?], name: String, upperBound: Predef.Class[?], args: scala.collection.Seq[Manifest[?]]) extends Manifest[T] {
    /** Returns the runtime class of the abstract type's upper bound, which serves as its erasure. */
    def runtimeClass = upperBound
    override val typeArguments = args.toList
    /** Returns the abstract type rendered as `prefix#name`, followed by `argString`: the type arguments
     *  if there are any, otherwise the bracketed component type of the upper bound if the upper bound
     *  is an array class, and otherwise nothing.
     */
    override def toString() = prefix.toString+"#"+name+argString
  }

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
   *  strictly necessary as it could be obtained by reflection. It was
   *  added so that erasure can be calculated without reflection.
   *
   *  @tparam T the type represented by this manifest
   *  @param prefix the manifest for the type containing this abstract type member
   *  @param name the name of the abstract type member
   *  @param upperBound the runtime `Class` of the upper bound, used to compute erasure without reflection
   *  @param args the manifests for the type arguments
   */
  def abstractType[T](prefix: Manifest[?], name: String, upperBound: Predef.Class[?], args: Manifest[?]*): Manifest[T] =
    new AbstractTypeManifest[T](prefix, name, upperBound, args)

  @SerialVersionUID(1L)
  private class WildcardManifest[T](lowerBound: Manifest[?], upperBound: Manifest[?]) extends Manifest[T] {
    /** Returns the runtime class of the wildcard's upper bound, which serves as its erasure. */
    def runtimeClass = upperBound.runtimeClass
    /** Returns the wildcard rendered as `_`, followed by ` >: ` and the lower bound and by ` <: `
     *  and the upper bound, each bound omitted when it is the `Nothing` manifest.
     */
    override def toString() =
      "_" +
        (if (lowerBound eq Nothing) "" else " >: "+lowerBound) +
        (if (upperBound eq Nothing) "" else " <: "+upperBound)
  }

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
   *
   *  @tparam T the type represented by this manifest
   *  @param lowerBound the manifest for the lower bound `L` of the wildcard
   *  @param upperBound the manifest for the upper bound `U` of the wildcard
   */
  def wildcardType[T](lowerBound: Manifest[?], upperBound: Manifest[?]): Manifest[T] =
    new WildcardManifest[T](lowerBound, upperBound)

  @SerialVersionUID(1L)
  private class IntersectionTypeManifest[T](parents: Array[Manifest[?]]) extends Manifest[T] {
    // We use an `Array` instead of a `Seq` for `parents` to avoid cyclic dependencies during deserialization
    // which can cause serialization proxies to leak and cause a ClassCastException.
    /** Returns the runtime class of the first type in the intersection, which serves as its erasure. */
    def runtimeClass = parents(0).runtimeClass
    /** Returns the types in the intersection rendered as `parents_0 with ... with parents_n`. */
    override def toString() = parents.mkString(" with ")
  }

  /** Manifest for the intersection type `parents_0 with ... with parents_n`.
   *
   *  @tparam T the type represented by this manifest
   *  @param parents the manifests for each type in the intersection
   */
  def intersectionType[T](parents: Manifest[?]*): Manifest[T] =
    new IntersectionTypeManifest[T](parents.toArray)
}
