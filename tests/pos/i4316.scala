object Test {

  case class Bar[A]

  def meth[A](consumer: A => Unit, s: Bar[A]): Unit = {
    s match {
      case bar: Bar[a] => {
        meth(consumer, new Bar[a]) // ok with `meth[a]`
      }
    }
  }
}
