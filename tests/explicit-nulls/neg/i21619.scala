def test1: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: Exception =>
  x.replace("", "") // error

def test2: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: Exception =>
      x = "e"
  x.replace("", "") // error

def test3: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: Exception =>
  finally
    x = "f"
  x.replace("", "") // ok

def test4: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    try
      if i == 1 then
        x = null
        throw new Exception()
      else
        x = ""
    catch
      case _ =>
    x = ""
  catch
    case _ =>
  x.replace("", "") // error