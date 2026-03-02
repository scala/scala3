package p

object P1:
  class ann(x: Int) extends annotation.Annotation

object P2:
  def id[T](x: T): T = x

object P3:
  @P1.ann(P2.id(22)) @main def blop = ()
