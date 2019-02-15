import language.strictEquality
def f[T](x: T) =
  if (x == null) ???        // error: cannot be compared
  else if (x == "abc") ???  // error: cannot be compared
  else ???