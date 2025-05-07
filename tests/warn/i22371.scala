//> using options -Werror -Wunused:all
import scala.compiletime.deferred

class Context

trait Foo:
  given context: Context = deferred
  given () => Context = deferred
