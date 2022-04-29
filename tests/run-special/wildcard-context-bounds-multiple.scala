

trait Conversion[A, B] {
  extension(a: A) def into(): B
}

given Conversion[String, Int] with
  extension(a: String) def into(): Int = a.toInt

def multipleTypeParamsNoSugar[T: Conversion[_, Int]](xs: List[T]): Int = xs.map(_.into()).reduce(_ + _)
def multipleTypeParams(xs: List[? : Conversion[_, Int]]): Int = xs.map(_.into()).reduce(_ + _)
type Into[B] = Conversion[_, B]
def multipleTypeParamsAlias(xs: List[? : Into[Int]]): Int = xs.map(_.into()).reduce(_ + _)


@main def Test =
  assert(multipleTypeParamsNoSugar(List("1", "2")) == 3)
  assert(multipleTypeParams(List("1", "2")) == 3)
  assert(multipleTypeParamsAlias(List("1", "2")) == 3)
