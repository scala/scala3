type X
val x: X = ???
extension (x: X) def unapply(arg: Any): Boolean = true

def testExtensionUnapply =
  ??? match
    case x() =>

def testBottomScrutinee =
  type Y = Nothing
  (??? : Y) match
    case Some(_) => 1
    case (_, _) => 2
