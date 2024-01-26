
def foo[T](f: T): T = ???

@main def main = foo:
  def f() = () // error !!! spurious error marker
  f(_) // error was OOM formatting TypeVar(TypeParamRef(T)) when offering explanations
