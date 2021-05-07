type Foo = Tuple2[Int, Int]
// case class Foo(x: Int, y: Int) // works
class Reader(m: deriving.Mirror.ProductOf[Foo])
given reader1(using m: deriving.Mirror.ProductOf[Foo]): Reader = new Reader(m)
inline def summonReader(): Reader = compiletime.summonInline[Reader]
@main def Test() = summonReader()
