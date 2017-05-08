
object Boo1 extends Phantom {
  class A extends this.Any // error
  class B extends this.Nothing // error
}
