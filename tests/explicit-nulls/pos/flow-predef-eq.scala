def f(s: String|Null): String = {
  if(s eq null) "foo" else s
}

def f2(s: String|Null): String = {
  if(s ne null) s else "foo"
}
