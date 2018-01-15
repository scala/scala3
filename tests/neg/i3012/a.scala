
object a extends fuz.Fuzbar {
  override def str = ""
  str()()()()()()  // error: missing argument
  str()() // error: missing argument
  str() // ok
  str // ok
}
