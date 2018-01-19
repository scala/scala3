class C { type T }

object Main {

  //val a: implicit Int => Int = implicit (x: Int) => x
  //val b: Int => Int = a

  def main(args: Array[String]): Unit = {
    val choose: implicit (c: C) => Set[Int] = Set.empty
    val b0: (C) => Set[Int] = choose
    val b1: (c: C) => Set[Int] = choose
    def applyF(f: (c: C) => Set[Int]) = f(new C{type T=Int})
    //applyF(choose)
  }
}
