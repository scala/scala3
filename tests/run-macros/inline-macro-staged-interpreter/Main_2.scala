
object Test {

  def main(args: Array[String]): Unit = {
    println(E.eval(I(2)))
    println(E.eval(new I(3)))
    println(E.eval(Plus(I(2), I(4))))
    println(E.eval(new Plus(I(3), I(4))))
    println(E.eval(Times(I(2), I(4))))
    println(E.eval(Times(I(2), Plus(I(3), I(4)))))

    println(E.eval(D(3.1)))
    println(E.eval(Times(D(2.4), Plus(D(3.9), D(4.6)))))
    println(E.eval(new Times(D(2.6), Plus(D(3.9), new D(4.1)))))
  }

}
