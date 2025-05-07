package scala

import scala.language.`2.13`

case class UninitializedFieldError(msg: String) extends RuntimeException(msg)
