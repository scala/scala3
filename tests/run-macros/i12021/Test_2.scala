
class X1(implicit i: Int)
class X2(using i: Int)

@main def Test = {
  println(inspect[X1])
  println(inspect[X2])
}