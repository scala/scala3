
object Boo extends Phantom {
  override def assume[P >: this.Nothing <: this.Any]: P = super.assume[P] // error
}
