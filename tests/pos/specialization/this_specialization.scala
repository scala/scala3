trait Foo[@specialized +A] {
// all those examples trigger bugs due to https://github.com/lampepfl/dotty/issues/592
  def bop[@specialized B >: A]: Foo[B] = new Bar[B](this)
  def gwa[@specialized B >: A]: Foo[B] = this
  def gwd[@specialized B >: A]: Foo[B] = {
    val d: Foo[B] = this
    d
  }
  //def bip[@specialized C >: A, @specialized D >: A]: Foo[D] = new Cho[D, C](new Bar[C](this))
}

case class Bar[@specialized a](tl: Foo[a]) extends Foo[a]

//case class Cho[@specialized c, @specialized d](tl: Bar[d]) extends Foo[c]
