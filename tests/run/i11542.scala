object demo {

  trait Reader[A]

  given Reader[Int]()

  inline def summonReader[T <: Tuple]: List[Reader[_]] = inline compiletime.erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => compiletime.summonInline[Reader[t]] :: summonReader[ts]
  }

  class CombinedReader[A](
    m: deriving.Mirror.ProductOf[A],
    childReaders: List[Reader[_]]
  ) extends Reader[A]

  inline given rdr: [A <: Tuple] => (m: deriving.Mirror.ProductOf[A]) => Reader[A] = {
    new CombinedReader(m, summonReader[m.MirroredElemTypes])
  }

}

@main def Test() = {
  // OK
  //summon[demo.Reader[(Int, Int, Int)]]

  // Exception in thread "main" java.lang.ClassCastException: class main$package$$anon$2 cannot be cast to class scala.deriving.Mirror$Product (main$package$$anon$2 and scala.deriving.Mirror$Product are in unnamed module of loader 'app')
  //      at main$package$.run(main.scala:25)
  //      at run.main(main.scala:23)
  summon[demo.Reader[(Int, (Int, Int))]]
}