package scala
import annotation.showAsInfix

sealed trait Tuple extends Any
object Tuple

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends Tuple

object *: