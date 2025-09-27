
// circe.scala

package io.circe

import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*

trait Encoder[A]:
  def encode(value: A): String

object Encoder:
  trait AsObject[A] extends Encoder[A]
  given Encoder[String] = ???

trait Configuration
object Configuration:
  val default: Configuration = ???

object Default:
  given [A]: Default[A] = ???
trait Default[T]

trait Codec[A] extends Encoder[A]
object Codec:
  trait AsObject[A] extends Encoder.AsObject[A]
  object AsObject:
    inline final def derived[A](using inline A: Mirror.Of[A]): Codec.AsObject[A] =
      ConfiguredCodec.derived[A](using Configuration.default)
    inline final def derivedConfigured[A](using
        inline A: Mirror.Of[A],
        inline conf: Configuration
    ): Codec.AsObject[A] = ConfiguredCodec.derived[A]

trait ConfiguredEncoder[A](using conf: Configuration) extends Encoder.AsObject[A]
trait ConfiguredCodec[A] extends Codec.AsObject[A], ConfiguredEncoder[A]
object ConfiguredCodec:
  inline final def derive[A: Mirror.Of](): ConfiguredCodec[A] =
    derived[A](using Configuration.default)
  inline final def derived[A](using
      conf: Configuration,
      inline mirror: Mirror.Of[A]
  ): ConfiguredCodec[A] = ${ derivedImpl[A]('conf, 'mirror) }
  def ofProduct[A](
      encoders: => List[Encoder[?]]
  )(using Configuration, Default[A]): ConfiguredCodec[A] = ???
  def derivedImpl[A: Type](conf: Expr[Configuration], mirror: Expr[Mirror.Of[A]])(using
      q: Quotes
  ): Expr[ConfiguredCodec[A]] = {
    mirror match {
      case '{
            ${ _ }: Mirror.ProductOf[A] {
              type MirroredLabel = l
              type MirroredElemLabels = el
              type MirroredElemTypes = et
            }
          } =>
        '{
          ConfiguredCodec.ofProduct[A](
            derivation.summonEncoders[et & Tuple](false)(using $conf)
          )(using $conf)
        }
    }
  }

object derivation:
  sealed trait Inliner[A, Arg]:
    inline def apply[T](inline arg: Arg): A

  class EncoderNotDeriveSum(using config: Configuration) extends Inliner[Encoder[?], Unit]:
    inline def apply[T](inline arg: Unit): Encoder[?] = summonEncoder[T](false)

  inline final def loopUnrolled[A, Arg, T <: Tuple](f: Inliner[A, Arg], inline arg: Arg): List[A] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: ts)  => f[h](arg) :: loopUnrolled[A, Arg, ts](f, arg)

  inline def loopUnrolledNoArg[A, T <: Tuple](f: Inliner[A, Unit]): List[A] =
    loopUnrolled[A, Unit, T](f, ())

  inline final def summonEncoders[T <: Tuple](inline derivingForSum: Boolean)(using
      Configuration
  ): List[Encoder[?]] =
    loopUnrolledNoArg[Encoder[?], T](
      inline if (derivingForSum) compiletime.error("unreachable")
      else new EncoderNotDeriveSum
    )

  private[circe] inline final def summonEncoder[A](
      inline derivingForSum: Boolean
  )(using Configuration): Encoder[A] = summonFrom {
      case encodeA: Encoder[A] => encodeA
      case _: Mirror.Of[A] =>
        inline if (derivingForSum) compiletime.error("unreachable")
        else error("Failed to find an instance of Encoder[]")
    }
