import java.nio.file.Files
import java.io.File

def repeated(s: String*) = ()

def f(s: String) = s

@main
def Test =
  repeated()
  repeated(f(""), "b")
  JavaVarargs_1.method()
  JavaVarargs_1.method("")

  var m = JavaVarargs_1.multiple("first")
  println(m)
  m = JavaVarargs_1.multiple(f("first"))
  println(m)
  m = JavaVarargs_1.multiple(f("first"), "a", "b", "c")
  println(m)
