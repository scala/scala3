import caps.*
trait A extends Stateful:
  def f() = /* read-only f */
    val ref: this.type = this
    this.g() // error
    ref.g()  // error

  update def g() = ???