trait Foo {
  // This should not generate a $init method.
  // Currently this needs to be verified by manual inspection of the bytecode.
  def meth(x: Int): Int
}
trait Bar extends Foo
trait Bam

class C extends Foo with Bar {
  def meth(x: Int) = x
}
