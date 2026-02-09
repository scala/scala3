import scala.language.experimental.erasedDefinitions

class EC extends compiletime.Erased

class X1(implicit i: Int)
class X2(using i: Int)
class X3(erased i: Int)
class X4(i: Int, erased j: Int)
class X5(i: EC)

@main def Test = {
  println(inspect[X1])
  println(inspect[X2])
  println(inspect[X3])
  println(inspect[X4])
  println(inspect[X5])
}
