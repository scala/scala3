sealed trait HList {
  dependent def ++(that: HList): HList =
    if (this.isInstanceOf[HNil.type]) that
    else HCons(this.asInstanceOf[HCons].head, this.asInstanceOf[HCons].tail ++ that)

  dependent def apply(index: Int): Int =
    if (index <= 0) this.asInstanceOf[HCons].head
    else this.asInstanceOf[HCons].tail.apply(index - 1)
}
dependent case class HCons(head: Int, tail: HList) extends HList
dependent case object HNil extends HList

object Test extends App {
  def main() = {
    dependent def xs0 = HCons(1, HCons(2, HCons(3, HCons(4, HCons(5, HCons(6, HCons(7, HCons(8, HCons(9, HCons(10, HCons(11, HCons(12, HCons(13, HCons(14, HCons(15, HCons(16, HNil))))))))))))))))
    xs0(15): 16

    dependent def xs1 = xs0 ++ xs0
    xs1(31): 16

    dependent def xs2 = xs1 ++ xs1
    xs2(63): 16

    dependent def xs3 = xs2 ++ xs2
    xs3(127): 16

    dependent def xs4 = xs3 ++ xs3
    xs4(255): 16

    // Add -Xss4M to javaOptions to compile with larger types...
    // dependent def xs5 = xs4 ++ xs4
    // xs5(511): 16

    // dependent def xs6 = xs5 ++ xs5
    // xs6(1023): 16

    // dependent def xs7 = xs6 ++ xs6
    // xs7(2047): 16

    // dependent def xs8 = xs7 ++ xs7
    // xs8(4095): 16 // ~5s
  }
}
