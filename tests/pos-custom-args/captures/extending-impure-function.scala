class F1 extends (Int => Unit) {
  def apply(x: Int): Unit = ()
}

class F2 extends (Int -> Unit) {
  def apply(x: Int): Unit = ()
}

def test =
  val x1 = new (Int => Unit) {
    def apply(x: Int): Unit = ()
  }

  val x2: Int -> Unit = new (Int => Unit) { // error
    def apply(x: Int): Unit = ()
  }

  val x3: Int -> Unit = new (Int -> Unit) {
    def apply(x: Int): Unit = ()
  }

  val y1: Int => Unit = new F1
  val y2: Int -> Unit = new F1 // error
  val y3: Int => Unit = new F2
  val y4: Int -> Unit = new F2

  val z1 = () => ()
  val z2: () -> Unit = () => ()
  val z3: () -> Unit = z1
  val z4: () => Unit = () => ()
