import caps.*
trait A extends Stateful:
  def f() = /* read-only f */
    val ref: this.type = this
    this.g()
    ref.g()

  update def g() = ???