trait ConfigSourceModule:
  object ConfigSource:
    class R

object M extends ConfigSourceModule

object Foo:
  implicit class FromConfigSource(c: M.ConfigSource.type)

object FooBar:                                      // problem disappears if we rename as `Bar`
  def foo: M.ConfigSource.R = new M.ConfigSource.R  // problem disappears if we use `???` as rhs