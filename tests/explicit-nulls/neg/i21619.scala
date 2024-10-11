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
  x.replace("", "") // error // LTS specific

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

def test5: Unit =
  var x: String | Null = null
  var y: String | Null = null
  x = ""
  y = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case _ =>
  val z1: String = x.replace("", "") // error
  val z2: String = y.replace("", "") // error // LTS specific