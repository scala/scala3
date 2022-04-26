trait Wrapper[T] {
    type Out
  }

  type Func[T] =
    T match {
      case String => Long
      case Long   => Int
      case Int    => Float
      case Float => Double
      case Double => Unit
      case Unit => String
    }

  implicit def infer[A]: Wrapper[One[A]] { type Out = Func[A] } = ???

  trait One[A] {
    def use(implicit w: Wrapper[One[A]]): One[w.Out]
  }

  val x: One[Long] = null
  val _ = x.use.use.use.use.use.use.use
