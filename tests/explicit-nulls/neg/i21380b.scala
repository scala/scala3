@main def test() = {
  var x: String | Null = null
  x = ""
  1 match {
    case 1 => x = null
  }
  x.replace("", "") // error
}
