object exec{
  trait Runner[T]{
    def run(t: T): Unit
  }
  object Runner{
    def run[T: Runner](t: T): Unit = implicitly[Runner[T]].run(t)
    implicit inline def runImplicitly[T]: Runner[T] = new {
      def run(t: T) = List(()).map(x => x).head // <<<
    }
  }
}
