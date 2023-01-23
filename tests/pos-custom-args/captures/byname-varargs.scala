def typeMismatch(addenda: => String*) = ???
class TypeMismatch(addenda: => String*)

def test =
  typeMismatch("foo")
  typeMismatch("foo", "bar")
  TypeMismatch("foo")
  TypeMismatch("foo", "bar")

