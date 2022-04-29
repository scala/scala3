import language.experimental.wildcardContextBounds

trait Showable[A] {
  extension(a: A) def show: String
}
object Showable

given Showable[String] with
  extension(a: String) def show: String = a

given Showable[Int] with
  extension(a: Int) def show: String = a.toString

def simple(xs: List[? : Showable]): String = xs.map(_.show).mkString(":")
def one(x: ? : Showable): String = x.show
def two(x: ? : Showable, y: ? : Showable): String = s"${x.show} :: ${y.show}"

def typeBounds(x: ? <: CharSequence : Showable): String = s"${x.show}!"
def existingTypeParam[T](x: T, y: ? : Showable): String = s"${x.toString} :: ${y.show}"
def bothKindsOfSugar[T: Showable](x: T, y: ? : Showable): String = s"${x.show} :: ${y.show}"

def showPair(p: (? : Showable, ? : Showable)): String = s"${p._1.show} :: ${p._2.show}"
def showLambda[I: Showable](i: I)(f: I => ? : Showable): String = s"${i.show} => ${f(i).show}"

class OnClass(u: ? : Showable) {
  def show: String = u.show
}


@main def Test =
  assert(simple(List(1, 2)) == "1:2")
  assert(one(1) == "1")
  assert(two("str", 1) == "str :: 1")
  assert(typeBounds("str") == "str!")
  assert(existingTypeParam(true, 1) == "true :: 1")
  assert(bothKindsOfSugar("str", 1) == "str :: 1")
  assert(showPair("str", 1) == "str :: 1")
  assert(showLambda("1")(s => s"'$s'") == "1 => '1'")
  assert(OnClass("str").show == "str")
