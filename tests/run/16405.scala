import scala.compiletime.summonInline

case class TypeDesc[T](tpe: String)
object TypeDesc {
  given nothing: TypeDesc[Nothing] = TypeDesc("Nothing")
  given string: TypeDesc[String] = TypeDesc("String")
  given int: TypeDesc[Int] = TypeDesc("Int")
}

def exampleFn(s: String, i: Int): Unit = ()

inline def argumentTypesOf[R](fun: (?, ?) => R): (TypeDesc[?], TypeDesc[?]) = {
  inline fun match {
    case x: ((a, b) => R) =>
      (scala.compiletime.summonInline[TypeDesc[a]], scala.compiletime.summonInline[TypeDesc[b]])
  }
}
inline def argumentTypesOfNoWildCard[A, B, R](fun: (A, B) => R): (TypeDesc[?], TypeDesc[?]) = argumentTypesOf(fun)
inline def argumentTypesOfAllWildCard(fun: (?, ?) => ?): (TypeDesc[?], TypeDesc[?]) = argumentTypesOf(fun)

object Test {
  def main(args: Array[String]): Unit = {
    val expected = (TypeDesc.string, TypeDesc.int)
    assert(argumentTypesOf(exampleFn) == expected)
    assert(argumentTypesOf(exampleFn(_, _)) == expected)
    assert(argumentTypesOfNoWildCard(exampleFn) == expected)
    assert(argumentTypesOfNoWildCard(exampleFn(_, _)) == expected)
    assert(argumentTypesOfAllWildCard(exampleFn) == expected)
    assert(argumentTypesOfAllWildCard(exampleFn(_, _)) == expected)
  } 
}