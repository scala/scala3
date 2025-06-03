//> using options -Wnonunit-statement -Werror

class Context

object Foo {
  def run(program: Context ?=> String): Unit = ???
}

def bar(using Context): String = ???

@main def run = Foo.run:
  bar // warn
  bar
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
