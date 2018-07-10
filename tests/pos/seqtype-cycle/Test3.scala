package scala

trait Seq[@specialized(1, 2) A]

class specialized(types: Int*) extends scala.annotation.StaticAnnotation
