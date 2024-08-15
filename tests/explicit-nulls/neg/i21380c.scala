def test1(i: Int): Int =
  var x: String | Null = null
  if i == 0 then x = ""
  else x = ""
  try
    x = x.replace(" ", "") // ok
    throw new Exception()
  catch
    case e: Exception =>
      x = x.replaceAll(" ", "") // error
      x = null
  x.length // error

def test2: Int =
  var x: String | Null = null
  try throw new Exception()
  finally x = ""
  x.length // ok

def test3 =
  var x: String | Null = ""
  try throw new Exception()
  catch case e: Exception =>
    x = (??? : String | Null)
  finally
    val l = x.length // error

def test4: Int =
  var x: String | Null = null
  try throw new Exception()
  catch
    case npe: NullPointerException => x = ""
    case _ => x = ""
  x.length // ok