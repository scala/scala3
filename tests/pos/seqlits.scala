import reflect.ClassTag

/** Some delayed computation like a Mill Task */
case class Task[T](body: () => T)

object SeqLits:

  /** A typeclass to map sequence literals with `T` elements
   *  to some collection type `C`.
   */
  trait FromArray[T, +C]:
    inline def fromArray(inline xs: IArray[T]): C

  /** An emulation of a sequence literal like [1, 2, 3]. Since we don't have that
   *  syntax yet, we express it here as `seqLit(1, 2, 3)`.
   */
  inline def seqLit[T: ClassTag, C](inline xs: T*)(using inline f: FromArray[T, C]): C =
    f.fromArray(IArray(xs*))

  /** Straightfoward mapping to Seq */
  given [T] => FromArray[T, Seq[T]]:
    inline def fromArray(inline xs: IArray[T]) = Seq(xs*)

  /** A more specific mapping to Vector */
  given [T] => FromArray[T, Vector[T]]:
    inline def fromArray(inline xs: IArray[T]) = Vector(xs*)

  /** A delaying mapping to Task */
  given [T] => FromArray[T, Task[Seq[T]]]:
    inline def fromArray(inline xs: IArray[T]) = Task(() => Seq(xs*))

  def last: Int = { println("last was evaluated"); 4 }

  val s: Seq[Int] = seqLit(1, 2, 3, last)
  val v: Vector[Int] = seqLit(1, 2, 3, last)
  val t: Task[Seq[Int]] = seqLit(1, 2, 3, last)

   @main def Test =
    println(s"Seq $s")
    println(s"Vector $v")
    println(s"${t.getClass.getSimpleName} with elems ${t.body()}")
