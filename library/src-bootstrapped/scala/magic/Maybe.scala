package scala.magic

import scala.magic.runtime.Valid
import annotation.experimental

/** Under experimental.magic, a trait backing maybe types `T?` */
@experimental
sealed trait Maybe[+T, +E] extends Any, Matchable:
  def isEmpty: Boolean
  def get: T




