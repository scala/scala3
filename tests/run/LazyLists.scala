package xcollections:
  import annotation.unchecked.uncheckedVariance
  import compiletime.uninitialized

  abstract class LazyList[+T]:

    private var myHead: T = uninitialized
    private var myTail: LazyList[T] = uninitialized
    private var myForced: LazyList[T] | Null = null

    protected def force(): LazyList[T]

    protected def set(hd: T @uncheckedVariance, tl: LazyList[T] @uncheckedVariance): LazyList[T] =
      assert(myForced == null, "implementation error: attempting to re-define existing LazyList")
      myHead = hd
      myTail = tl
      this

    def forced(): LazyList[T] =
      var myForced = this.myForced
      if myForced == null then
        myForced = force()
        this.myForced = myForced
        assert(myForced.myForced != null, "implementation error: LazyList was not forced")
        myForced
      else myForced

    def isEmpty: Boolean = forced() eq LazyList.empty

    def head: T =
      val e = forced()
      require(!e.isEmpty, "head on empty LazyList")
      e.myHead

    def tail: LazyList[T] =
      val e = forced()
      require(!e.isEmpty, "tail on empty LazyList")
      e.myTail

    def fromIterable[T](xs: Iterable[T]) = xs match
      case xs: LazyList[T] @unchecked => xs
      case _ => LazyList.fromIterator(xs.iterator)

  object LazyList:

    val empty: LazyList[Nothing] = new:
      protected def force(): LazyList[Nothing] = this

    object `#::`:
      def unapply[T](xs: LazyList[T]): Option[(T, LazyList[T])] =
        if xs.isEmpty then None
        else Some((xs.head, xs.tail))

    def fromIterator[T](it: Iterator[T]): LazyList[T] = new:
      protected def force() =
        if it.hasNext then set(it.next, fromIterator  (it))
        else empty

    extension [T, U >: T](xs: LazyList[T])
      def #::(x: U): LazyList[U] = new:
        protected def force(): LazyList[U] =
          set(x, xs)

      def ++(ys: LazyList[U]): LazyList[U] = new:
        protected def force() =
          if xs.isEmpty then ys.forced()
          else set(xs.head, xs.tail ++ ys)

    extension [T, U](xs: LazyList[T])
      def map(f: T => U): LazyList[U] = new:
        protected def force() =
          if xs.isEmpty then empty
          else set(f(xs.head), xs.tail.map(f))

      def flatMap(f: T => LazyList[U]): LazyList[U] = new:
        protected def force(): LazyList[U] =
          if xs.isEmpty then empty
          else f(xs.head) ++ xs.tail.flatMap(f)

      def foldLeft(z: U)(f: (U, T) => U): U =
        if xs.isEmpty then z
        else xs.tail.foldLeft(f(z, xs.head))(f)

    extension [T](xs: LazyList[T])
      def filter(p: T => Boolean): LazyList[T] = new:
        protected def force(): LazyList[T] =
          if xs.isEmpty then empty
          else if p(xs.head) then set(xs.head, xs.tail.filter(p))
          else xs.tail.filter(p)

      def take(n: Int): LazyList[T] =
        if n <= 0 then empty
        else new:
          protected def force(): LazyList[T] =
            if xs.isEmpty then xs
            else set(xs.head, xs.tail.take(n - 1))

      def drop(n: Int): LazyList[T] =
        if n <= 0 then xs
        else new:
          protected def force(): LazyList[T] =
            def advance(xs: LazyList[T], n: Int): LazyList[T] =
              if n <= 0 || xs.isEmpty then xs
              else advance(xs.tail, n - 1)
            advance(xs, n)

  end LazyList
end xcollections

@main def Test() = ()