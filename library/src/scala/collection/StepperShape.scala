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
  * specialized Stepper `S` according to the element type `T`.
  */
sealed trait StepperShape[-T, S <: Stepper[?]] { self =>
  /** Returns the Int constant (as defined in the `StepperShape` companion object) for this `StepperShape`. */
  def shape: StepperShape.Shape

  /** Creates an unboxing primitive sequential Stepper from a boxed `AnyStepper`.
   * This is an identity operation for reference shapes. */
  def seqUnbox(st: AnyStepper[T]^): S^{st}

  /** Creates an unboxing primitive parallel (i.e. `with EfficientSplit`) Stepper from a boxed `AnyStepper`.
   * This is an identity operation for reference shapes. */
  def parUnbox(st: (AnyStepper[T] & EfficientSplit)^): (S & EfficientSplit)^{st}
}

object StepperShape extends StepperShapeLowPriority1 {
  class Shape private[StepperShape] (private val s: Int) extends AnyVal

  // reference
  val ReferenceShape = new Shape(0)

  // primitive
  val IntShape    = new Shape(1)
  val LongShape   = new Shape(2)
  val DoubleShape = new Shape(3)

  // widening
  val ByteShape  = new Shape(4)
  val ShortShape = new Shape(5)
  val CharShape  = new Shape(6)
  val FloatShape = new Shape(7)

  implicit val intStepperShape: StepperShape[Int, IntStepper] = new StepperShape[Int, IntStepper] {
    def shape = IntShape
    def seqUnbox(st: AnyStepper[Int]^): IntStepper^{st} = new Stepper.UnboxingIntStepper(st)
    def parUnbox(st: (AnyStepper[Int] & EfficientSplit)^): (IntStepper & EfficientSplit)^{st} = new Stepper.UnboxingIntStepper(st) with EfficientSplit
  }
  implicit val jIntegerStepperShape: StepperShape[jl.Integer, IntStepper] = intStepperShape.asInstanceOf[StepperShape[jl.Integer, IntStepper]]

  implicit val longStepperShape: StepperShape[Long, LongStepper] = new StepperShape[Long, LongStepper] {
    def shape = LongShape
    def seqUnbox(st: AnyStepper[Long]^): LongStepper^{st} = new Stepper.UnboxingLongStepper(st)
    def parUnbox(st: (AnyStepper[Long] & EfficientSplit)^): (LongStepper & EfficientSplit)^{st} = new Stepper.UnboxingLongStepper(st) with EfficientSplit
  }
  implicit val jLongStepperShape: StepperShape[jl.Long, LongStepper] = longStepperShape.asInstanceOf[StepperShape[jl.Long, LongStepper]]

  implicit val doubleStepperShape: StepperShape[Double, DoubleStepper] = new StepperShape[Double, DoubleStepper] {
    def shape = DoubleShape
    def seqUnbox(st: AnyStepper[Double]^): DoubleStepper^{st} = new Stepper.UnboxingDoubleStepper(st)
    def parUnbox(st: (AnyStepper[Double] & EfficientSplit)^): (DoubleStepper & EfficientSplit)^{st} = new Stepper.UnboxingDoubleStepper(st) with EfficientSplit
  }
  implicit val jDoubleStepperShape: StepperShape[jl.Double, DoubleStepper] = doubleStepperShape.asInstanceOf[StepperShape[jl.Double, DoubleStepper]]

  implicit val byteStepperShape: StepperShape[Byte, IntStepper] = new StepperShape[Byte, IntStepper] {
    def shape = ByteShape
    def seqUnbox(st: AnyStepper[Byte]^): IntStepper^{st} = new Stepper.UnboxingByteStepper(st)
    def parUnbox(st: (AnyStepper[Byte] & EfficientSplit)^): (IntStepper & EfficientSplit)^{st} = new Stepper.UnboxingByteStepper(st) with EfficientSplit
  }
  implicit val jByteStepperShape: StepperShape[jl.Byte, IntStepper] = byteStepperShape.asInstanceOf[StepperShape[jl.Byte, IntStepper]]

  implicit val shortStepperShape: StepperShape[Short, IntStepper] = new StepperShape[Short, IntStepper] {
    def shape = ShortShape
    def seqUnbox(st: AnyStepper[Short]^): IntStepper^{st} = new Stepper.UnboxingShortStepper(st)
    def parUnbox(st: (AnyStepper[Short] & EfficientSplit)^): (IntStepper & EfficientSplit)^{st} = new Stepper.UnboxingShortStepper(st) with EfficientSplit
  }
  implicit val jShortStepperShape: StepperShape[jl.Short, IntStepper] = shortStepperShape.asInstanceOf[StepperShape[jl.Short, IntStepper]]

  implicit val charStepperShape: StepperShape[Char, IntStepper] = new StepperShape[Char, IntStepper] {
    def shape = CharShape
    def seqUnbox(st: AnyStepper[Char]^): IntStepper^{st} = new Stepper.UnboxingCharStepper(st)
    def parUnbox(st: (AnyStepper[Char] & EfficientSplit)^): (IntStepper & EfficientSplit)^{st} = new Stepper.UnboxingCharStepper(st) with EfficientSplit
  }
  implicit val jCharacterStepperShape: StepperShape[jl.Character, IntStepper] = charStepperShape.asInstanceOf[StepperShape[jl.Character, IntStepper]]

  implicit val floatStepperShape: StepperShape[Float, DoubleStepper] = new StepperShape[Float, DoubleStepper] {
    def shape = FloatShape
    def seqUnbox(st: AnyStepper[Float]^): DoubleStepper^{st} = new Stepper.UnboxingFloatStepper(st)
    def parUnbox(st: (AnyStepper[Float] & EfficientSplit)^): (DoubleStepper & EfficientSplit)^{st} = new Stepper.UnboxingFloatStepper(st) with EfficientSplit
  }
  implicit val jFloatStepperShape: StepperShape[jl.Float, DoubleStepper] = floatStepperShape.asInstanceOf[StepperShape[jl.Float, DoubleStepper]]
}

trait StepperShapeLowPriority1 extends StepperShapeLowPriority2 {
  implicit def anyStepperShape[T]: StepperShape[T, AnyStepper[T]] = anyStepperShapePrototype.asInstanceOf[StepperShape[T, AnyStepper[T]]]
}

trait StepperShapeLowPriority2 {
  implicit def baseStepperShape[T]: StepperShape[T, Stepper[T]] = anyStepperShapePrototype.asInstanceOf[StepperShape[T, Stepper[T]]]

  protected val anyStepperShapePrototype: StepperShape[AnyRef, Stepper[AnyRef]] = new StepperShape[AnyRef, Stepper[AnyRef]] {
    def shape = StepperShape.ReferenceShape
    def seqUnbox(st: AnyStepper[AnyRef]^): Stepper[AnyRef]^{st} = st
    def parUnbox(st: (AnyStepper[AnyRef] & EfficientSplit)^): (Stepper[AnyRef] & EfficientSplit)^{st} = st
  }
}
