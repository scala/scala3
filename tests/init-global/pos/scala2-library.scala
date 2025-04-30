package scala

import scala.language.`2`

case class UninitializedFieldError(msg: String) extends RuntimeException(msg)
