object Boo extends Phantom {
  type BooAny = this.Any
  def assume1: BooAny = assume() // error:  method assume in trait Phantom does not take parameters
}
