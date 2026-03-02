trait B[-A, +To] {
  def addOne(e: A): this.type = this
  def res(): To
}

class Col[A]

object Factory {
  def newB[A](using reflect.ClassTag[A]) = new B[A, Col[A]] { def res(): Col[A] = new Col[A] }
}

def test =
  val a = Factory.newB.addOne(1).res()
  val b = collection.immutable.ArraySeq.newBuilder.addOne(1).result()
