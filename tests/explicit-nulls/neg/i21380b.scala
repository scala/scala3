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