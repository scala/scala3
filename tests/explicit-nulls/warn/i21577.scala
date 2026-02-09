def f(s: String) =
  val s2 = s.trim()
  s2 match
    case s3: String =>
    case _ => // warn: null only


def f2(s: String | Null) =
  val s2 = s.nn.trim()
  s2 match
    case s3: String =>
    case _ => // warn: null only

def f3(s: String | Null) = s match
  case s2: String =>
  case _ => // warn: null only

def f5(s: String) = s match
  case _: String =>
  case _ => // warn: unreachable

def f6(s: String) = s.trim() match
  case _: String =>
  case null =>

def f61(s: String) = s.trim() match
  case _: String =>

def f7(s: String | Null) = s match // warn: not exhaustive
  case _: String =>

def f8(s: String | Null) = s match
  case _: String =>
  case null =>

def f9(s: String | Int | Null) = s match // warn: not exhaustive
  case _: String =>
  case null =>
