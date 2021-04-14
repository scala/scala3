import scala.language.experimental.erasedDefinitions

class X1(implicit i: Int)
class X2(using i: Int)
class X3(erased i: Int)

@main def Test = {
  println(inspect[X1])
  println(inspect[X2])
  println(inspect[X3])
}