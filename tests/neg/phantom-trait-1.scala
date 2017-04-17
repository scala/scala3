
object Boo extends Phantom {
  override val assume: this.Nothing = super.assume // error
}
