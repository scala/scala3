@main def test =
  val myregex_r = "\\s+".r
  val text = "adggfgf dfg"
  myregex_r findFirstMatchIn text
  text takeRight 5
  val func = (a: Int) => a + 1
  List(1,2, 3) map func
  text stripPrefix "adgg"

