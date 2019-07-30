import annotation.meta.cached
object Test {

  @cached val x: Int = 1 // error: @cached annotation cannot be applied
  @cached type T = Int // error: @cached annotation cannot be applied
  @cached class Foo // error: @cached annotation cannot be applied
  @cached object Bar // error: @cached annotation cannot be applied

  @cached def foo(x: Int) = x // error: @cached method cannot have regular parameters
  @cached def foo[T]: T = ??? // error: @cached method cannot have type parameters
}