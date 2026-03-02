@main def test() = {
  var x: String | Null = null
  if (false) {
    x = ""

  } else {
    x = ""
  }
  try {
    x = ""
    throw new Exception()
  }
  catch {
    case e: Exception => {
      x = null
    }
  }
  x.replace("", "") // error
}
