package dotty.tools.dotc.util

opaque type UniqList[+A] >: Null <: AnyRef = List[A]

object UniqList:
  def empty: UniqList[Nothing] = Nil
  extension [A](xs: UniqList[A])
    def foreach(f: A => Unit): Unit          = xs.foreach(f)
    def filter(p: A => Boolean): UniqList[A] = xs.filter(p)
    def + (x: A): UniqList[A]                = if xs.contains(x) then xs else xs :+ x
    def & (ys: UniqList[A]): UniqList[A]     = xs.filter(ys.contains)
    def | (ys: UniqList[A]): UniqList[A]     = (xs ::: ys).distinct
    def toList: List[A]                      = xs

  class Builder[A] extends scala.collection.mutable.Builder[A, UniqList[A]]:
    private val xs = new scala.collection.mutable.ListBuffer[A]
    override def addOne(x: A): this.type                = { xs += x; this }
    override def addAll(ys: IterableOnce[A]): this.type = { xs ++= ys; this }
    override def clear(): Unit                          = xs.clear()
    override def result(): UniqList[A]                  = xs.distinct.toList
    override def sizeHint(size: Int)                    = xs.sizeHint(size)
