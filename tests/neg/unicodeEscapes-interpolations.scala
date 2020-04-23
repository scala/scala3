object Example {
  val badInters1 = s"foo \unope that's wrong" // error
  val badIntersEnd1 = s"foo \u12" // error
  val badInters3 = s"""foo \unope that's wrong""" // error
  val caretPos1 = s"foo \u12x3 pos @ x" // error
  val caretPos2 = s"foo \uuuuuuu12x3 pos @ x" // error
  val caretPos3 = s"""foo \u12x3 pos @ x""" // error
  val caretPos4 = s"""foo \uuuuuuu12x3 pos @ x""" // error
  val placeholder = "place"
  val badIntersmultiAfter = s"foo $placeholder bar \unope that's wrong" // error
  val badIntersmultiBefore = s"foo \unope $placeholder that's wrong" // error
  val badInterstmultiAfter = s"""foo $placeholder bar \unope that's wrong""" // error
  val badInterstmultiBefore = s"""foo \unope $placeholder that's wrong""" // error
  val badInterother = s"this \p ain't legal either" // error
}