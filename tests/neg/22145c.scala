package foo

trait Collection:
  def bar(base: Collection) = base.foo // error
  object a extends Collection:
    def foo: Int = 0
  object b extends Collection:
    def foo: Int = 1
