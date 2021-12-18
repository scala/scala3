package example

import language.`3.0-migration`

sealed abstract class ErrorMessageID($name: String, _$ordinal: Int)
  extends java.lang.Enum[ErrorMessageID]($name, _$ordinal) with scala.reflect.Enum {

  def errorNumber: Int = this.ordinal() - 2
}

object ErrorMessageID extends scala.reflect.EnumCompanion[ErrorMessageID]{

  final val LazyErrorId = $new(0, "LazyErrorId")
  final val NoExplanationID = $new(1, "NoExplanationID")

  private[this] val $values: Array[ErrorMessageID] =
    Array(this.LazyErrorId, this.NoExplanationID) // error // error

  def values: Array[ErrorMessageID] = $values.clone()

  def valueOf($name: String): ErrorMessageID = $name match {
    case "LazyErrorId" => this.LazyErrorId
    case "NoExplanationID" => this.NoExplanationID
    case _ => throw new IllegalArgumentException("enum case not found: " + $name)
  }

  private[this] def $new(_$ordinal: Int, $name: String): ErrorMessageID =
    new ErrorMessageID($name, _$ordinal) with scala.runtime.EnumValue {
        override def productPrefix: String = this.name()
    }

  def fromOrdinal(ordinal: Int): ErrorMessageID =
    try ErrorMessageID.$values.apply(ordinal)
    catch { case _ => throw new NoSuchElementException(ordinal.toString()) }
}