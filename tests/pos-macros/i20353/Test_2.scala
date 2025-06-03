//> using options -experimental

class OuterClass:
  @Test
  class InnerClass

  @Test
  object InnerObject
end OuterClass

object OuterObject:
  @Test
  class InnerClass

  @Test
  object InnerObject
end OuterObject
