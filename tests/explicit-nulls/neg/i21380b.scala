def test1 =
  var x: String | Null = null
  x = ""
  1 match
    case 1 => x = null
    case _ => x = x.trim() // ok
  x.replace("", "") // error

def test2(i: Int) =
  var x: String | Null = null
  i match
    case 1 => x = "1"
    case _ => x = " "
  x.replace("", "") // ok

def test3(i: Int) =
  var x: String | Null = null
  i match
    case 1 if x != null => ()
    case _ => x = " "
  x.trim() // ok

def test4(i: Int) =
  var x: String | Null = null
  var y: String | Null = null
  i match
    case 1 => x = "1"
    case _ => y = " "
  x.trim() // error

def test5(i: Int): String =
  var x: String | Null = null
  var y: String | Null = null
  i match
    case 1 => x = "1"
    case _ =>
      y = " "
      return y
  x.trim() // ok