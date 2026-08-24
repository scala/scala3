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

package scala.collection

import java.{lang => jl}

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.collection.Stepper.EfficientSplit

/** An implicit StepperShape instance is used in the [[IterableOnce.stepper]] to return a possibly
 *  specialized Stepper `S` according to the element type `T`.
 *
 *  @tparam T the element type of the collection (may be a primitive or reference type)
 *  @tparam S the type of `Stepper` to use, possibly specialized for primitive types
 */
sealed trait StepperShape[-T, S <: Stepper[?]] { self =>
  /** Returns the Int constant (as defined in the `StepperShape` companion object) for this `StepperShape`. */
  def shape: StepperShape.Shape

  /** Creates an unboxing primitive sequential Stepper from a boxed `AnyStepper`.
   *  This is an identity operation for reference shapes. 
   *
   *  @param st the boxed `AnyStepper` to convert into a possibly specialized stepper
   *  @return a sequential `Stepper` of shape `S` that unboxes elements from `st`, or `st` itself for reference shapes
   */
  def seqUnbox(st: AnyStepper[T]^): S^{st}

  /** Creates an unboxing primitive parallel (i.e. `with EfficientSplit`) Stepper from a boxed `AnyStepper`.
   *  This is an identity operation for reference shapes. 
   *
   *  @param st the boxed `AnyStepper` with `EfficientSplit` capability to convert into a possibly specialized stepper
   *  @return a parallel `Stepper` of shape `S` with `EfficientSplit` that unboxes elements from `st`, or `st` itself for reference shapes
   */
  def parUnbox(st: (AnyStepper[T] & EfficientSplit)^): (S & EfficientSplit)^{st}
}

object StepperShape extends StepperShapeLowPriority1 {
  /** A value class enumerating the possible shapes of a [[StepperShape]]. The values are
   *  the constants defined in this companion object, [[ReferenceShape]] through [[FloatShape]].
   */
  class Shape private[StepperShape] (private val s: Int) extends AnyVal

  // reference
  /** The shape for reference (boxed) element types, stepped over with an [[AnyStepper]]. */
  val ReferenceShape = new Shape(0)

  // primitive
  /** The shape for `Int` elements, stepped over with an [[IntStepper]]. */
  val IntShape    = new Shape(1)
  /** The shape for `Long` elements, stepped over with a [[LongStepper]]. */
  val LongShape   = new Shape(2)
  /** The shape for `Double` elements, stepped over with a [[DoubleStepper]]. */
  val DoubleShape = new Shape(3)

  // widening
  /** The shape for `Byte` elements, widened to `Int` and stepped over with an [[IntStepper]]. */
  val ByteShape  = new Shape(4)
  /** The shape for `Short` elements, widened to `Int` and stepped over with an [[IntStepper]]. */
  val ShortShape = new Shape(5)
  /** The shape for `Char` elements, widened to `Int` and stepped over with an [[IntStepper]]. */
  val CharShape  = new Shape(6)
  /** The shape for `Float` elements, widened to `Double` and stepped over with a [[DoubleStepper]]. */
  val FloatShape = new Shape(7)

  /** The `StepperShape` for `Int` elements, selecting [[IntStepper]]. */
  implicit val intStepperShape: StepperShape[Int, IntStepper] = new StepperShape[Int, IntStepper] {
    def shape = IntShape
    def seqUnbox(st: AnyStepper[Int]^): IntStepper^{st} = new Stepper.UnboxingIntStepper(st)
    def parUnbox(st: (AnyStepper[Int] & EfficientSplit)^): (IntStepper & EfficientSplit)^{st} = new Stepper.UnboxingIntStepper(st) with EfficientSplit
  }
  /** The `StepperShape` for `java.lang.Integer` elements, selecting [[IntStepper]]; the same
   *  instance as [[intStepperShape]].
   */
  implicit val jIntegerStepperShape: StepperShape[jl.Integer, IntStepper] = intStepperShape.asInstanceOf[StepperShape[jl.Integer, IntStepper]]

  /** The `StepperShape` for `Long` elements, selecting [[LongStepper]]. */
  implicit val longStepperShape: StepperShape[Long, LongStepper] = new StepperShape[Long, LongStepper] {
    def shape = LongShape
    def seqUnbox(st: AnyStepper[Long]^): LongStepper^{st} = new Stepper.UnboxingLongStepper(st)
    def parUnbox(st: (AnyStepper[Long] & EfficientSplit)^): (LongStepper & EfficientSplit)^{st} = new Stepper.UnboxingLongStepper(st) with EfficientSplit
  }
  /** The `StepperShape` for `java.lang.Long` elements, selecting [[LongStepper]]; the same
   *  instance as [[longStepperShape]].
   */
  implicit val jLongStepperShape: StepperShape[jl.Long, LongStepper] = longStepperShape.asInstanceOf[StepperShape[jl.Long, LongStepper]]

  /** The `StepperShape` for `Double` elements, selecting [[DoubleStepper]]. */
  implicit val doubleStepperShape: StepperShape[Double, DoubleStepper] = new StepperShape[Double, DoubleStepper] {
    def shape = DoubleShape
    def seqUnbox(st: AnyStepper[Double]^): DoubleStepper^{st} = new Stepper.UnboxingDoubleStepper(st)
    def parUnbox(st: (AnyStepper[Double] & EfficientSplit)^): (DoubleStepper & EfficientSplit)^{st} = new Stepper.UnboxingDoubleStepper(st) with EfficientSplit
  }
  /** The `StepperShape` for `java.lang.Double` elements, selecting [[DoubleStepper]]; the same
   *  instance as [[doubleStepperShape]].
   */
  implicit val jDoubleStepperShape: StepperShape[jl.Double, DoubleStepper] = doubleStepperShape.asInstanceOf[StepperShape[jl.Double, DoubleStepper]]

  /** The `StepperShape` for `Byte` elements, selecting [[IntStepper]]; the elements are
   *  widened to `Int`.
   */
  implicit val byteStepperShape: StepperShape[Byte, IntStepper] = new StepperShape[Byte, IntStepper] {
    def shape = ByteShape
    def seqUnbox(st: AnyStepper[Byte]^): IntStepper^{st} = new Stepper.UnboxingByteStepper(st)
    def parUnbox(st: (AnyStepper[Byte] & EfficientSplit)^): (IntStepper & EfficientSplit)^{st} = new Stepper.UnboxingByteStepper(st) with EfficientSplit
  }
  /** The `StepperShape` for `java.lang.Byte` elements, selecting [[IntStepper]]; the same
   *  instance as [[byteStepperShape]].
   */
  implicit val jByteStepperShape: StepperShape[jl.Byte, IntStepper] = byteStepperShape.asInstanceOf[StepperShape[jl.Byte, IntStepper]]

  /** The `StepperShape` for `Short` elements, selecting [[IntStepper]]; the elements are
   *  widened to `Int`.
   */
  implicit val shortStepperShape: StepperShape[Short, IntStepper] = new StepperShape[Short, IntStepper] {
    def shape = ShortShape
    def seqUnbox(st: AnyStepper[Short]^): IntStepper^{st} = new Stepper.UnboxingShortStepper(st)
    def parUnbox(st: (AnyStepper[Short] & EfficientSplit)^): (IntStepper & EfficientSplit)^{st} = new Stepper.UnboxingShortStepper(st) with EfficientSplit
  }
  /** The `StepperShape` for `java.lang.Short` elements, selecting [[IntStepper]]; the same
   *  instance as [[shortStepperShape]].
   */
  implicit val jShortStepperShape: StepperShape[jl.Short, IntStepper] = shortStepperShape.asInstanceOf[StepperShape[jl.Short, IntStepper]]

  /** The `StepperShape` for `Char` elements, selecting [[IntStepper]]; the elements are
   *  widened to `Int`.
   */
  implicit val charStepperShape: StepperShape[Char, IntStepper] = new StepperShape[Char, IntStepper] {
    def shape = CharShape
    def seqUnbox(st: AnyStepper[Char]^): IntStepper^{st} = new Stepper.UnboxingCharStepper(st)
    def parUnbox(st: (AnyStepper[Char] & EfficientSplit)^): (IntStepper & EfficientSplit)^{st} = new Stepper.UnboxingCharStepper(st) with EfficientSplit
  }
  /** The `StepperShape` for `java.lang.Character` elements, selecting [[IntStepper]]; the same
   *  instance as [[charStepperShape]].
   */
  implicit val jCharacterStepperShape: StepperShape[jl.Character, IntStepper] = charStepperShape.asInstanceOf[StepperShape[jl.Character, IntStepper]]

  /** The `StepperShape` for `Float` elements, selecting [[DoubleStepper]]; the elements are
   *  widened to `Double`.
   */
  implicit val floatStepperShape: StepperShape[Float, DoubleStepper] = new StepperShape[Float, DoubleStepper] {
    def shape = FloatShape
    def seqUnbox(st: AnyStepper[Float]^): DoubleStepper^{st} = new Stepper.UnboxingFloatStepper(st)
    def parUnbox(st: (AnyStepper[Float] & EfficientSplit)^): (DoubleStepper & EfficientSplit)^{st} = new Stepper.UnboxingFloatStepper(st) with EfficientSplit
  }
  /** The `StepperShape` for `java.lang.Float` elements, selecting [[DoubleStepper]]; the same
   *  instance as [[floatStepperShape]].
   */
  implicit val jFloatStepperShape: StepperShape[jl.Float, DoubleStepper] = floatStepperShape.asInstanceOf[StepperShape[jl.Float, DoubleStepper]]
}

/** Low-priority implicit `StepperShape` instances, tried after the specialized primitive
 *  instances defined in the [[StepperShape]] companion object.
 */
trait StepperShapeLowPriority1 extends StepperShapeLowPriority2 {
  /** Returns the `StepperShape` for an arbitrary element type `T`, selecting `AnyStepper[T]`
   *  with [[StepperShape.ReferenceShape]].
   *
   *  @tparam T the element type
   *  @return the reference-shape instance [[StepperShapeLowPriority2.anyStepperShapePrototype]], cast to `StepperShape[T, AnyStepper[T]]`
   */
  implicit def anyStepperShape[T]: StepperShape[T, AnyStepper[T]] = anyStepperShapePrototype.asInstanceOf[StepperShape[T, AnyStepper[T]]]
}

/** Lowest-priority implicit `StepperShape` instances, tried after those in
 *  [[StepperShapeLowPriority1]].
 */
trait StepperShapeLowPriority2 {
  /** Returns the `StepperShape` for an arbitrary element type `T`, selecting the base type
   *  `Stepper[T]` with [[StepperShape.ReferenceShape]].
   *
   *  @tparam T the element type
   *  @return the reference-shape instance [[anyStepperShapePrototype]], cast to `StepperShape[T, Stepper[T]]`
   */
  implicit def baseStepperShape[T]: StepperShape[T, Stepper[T]] = anyStepperShapePrototype.asInstanceOf[StepperShape[T, Stepper[T]]]

  /** The single reference-shape instance underlying [[anyStepperShape]] and
   *  [[baseStepperShape]]; its `seqUnbox` and `parUnbox` return the given stepper unchanged.
   */
  protected val anyStepperShapePrototype: StepperShape[AnyRef, Stepper[AnyRef]] = new StepperShape[AnyRef, Stepper[AnyRef]] {
    def shape = StepperShape.ReferenceShape
    def seqUnbox(st: AnyStepper[AnyRef]^): Stepper[AnyRef]^{st} = st
    def parUnbox(st: (AnyStepper[AnyRef] & EfficientSplit)^): (Stepper[AnyRef] & EfficientSplit)^{st} = st
  }
}
