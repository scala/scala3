sealed abstract class Foo[@specialized +A] {
  def bop[@specialized B >: A]: Foo[B] = new Bar[B](this)
  //def bip[@specialized C >: A, @specialized D >: A]: Foo[D] = new Cho[D, C](new Bar[C](this))
}

case class Bar[@specialized a](tl: Foo[a]) extends Foo[a]

//case class Cho[@specialized c, @specialized d](tl: Bar[d]) extends Foo[c]