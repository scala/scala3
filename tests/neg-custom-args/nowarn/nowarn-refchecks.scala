import annotation.nowarn

@deprecated def f = 1

def t1 = f // error

@nowarn("cat=deprecation") def t2 = f
@nowarn("msg=deprecated") def t3 = f
@nowarn("msg=fish") // error, unused nowarn
def t4 = f // error
@nowarn("") def t5 = f
@nowarn def t6 = f

def t7 = f: @nowarn("cat=deprecation")
def t8 = f:
  @nowarn("msg=deprecated")
def t9 = f: // error
  @nowarn("msg=fish") // error, unused nowarn
def t10 = f: @nowarn("")
def t11 = f: @nowarn

// unused nowarn; this test only runs until refchecks with Werror, the unchecked warning is issued in a later phase
@nowarn("cat=unchecked") def t7b(x: Any) = x match // error
  case _: List[Int] => 0
  case _ => 1
