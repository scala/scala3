//> using options -language:experimental.inlineTraits -Werror -deprecation

inline trait Parent:
  @deprecated("foo1", "1.0")
  def foo: Int = 1

trait Child extends Parent

trait Grandchild extends Child:
  
  @deprecated("foo2", "2.0")
  override def foo: Int = 42
