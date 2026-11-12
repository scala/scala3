class ShadowIt {
  def foo = {
    val this = "shadow" // error
    this
  }
}
