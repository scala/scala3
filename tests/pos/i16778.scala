final abstract class ForcedRecompilationToken[T]

object ForcedRecompilationToken {
  implicit def materialize: ForcedRecompilationToken["x"] = null.asInstanceOf[ForcedRecompilationToken["x"]]
}

class PluginDef[T](implicit val recompilationToken: ForcedRecompilationToken[T])

object X {
  val no = {
    final class anon extends PluginDef {} // was: missing type parameters
    new anon
  }

  val bad = new PluginDef {} // was: No given instance
  val good = new PluginDef() {} // ok
}

object DependingPlugin {
  class NestedDoublePlugin extends PluginDef
  object NestedDoublePlugin extends PluginDef
}
