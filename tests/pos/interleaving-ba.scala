
object BA {
    given String = ""
    given Double = 0

    def ba[A](x: A)[B](using B): B = summon[B]

    def test = ba(0)[String]

}
