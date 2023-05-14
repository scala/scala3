package lib

sealed trait Top
object Top // companion is necessary

case class Middle() extends Top with Bottom
sealed trait Bottom extends Top
