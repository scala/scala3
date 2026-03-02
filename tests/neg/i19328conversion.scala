object i19328conversion:

  trait Foo[B]
  given foo[C]: Foo[C] = new Foo[C] {}

  type Id[A] = A

  given wrapId: [A] => Conversion[A, Id[A]]:
    def apply(x: A): Id[A] = x

  def bar(using bool: Boolean): Unit = ()

  bar // error: missing implicit (should not crash)
