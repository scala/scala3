
// val world = "world"
// @new("world", true) @add("world")
// val hello = "Hello" + world
// @hello @new("good", false) @add("hello") val info = "A:" + hello
// val good = "good"

// @add("good")
// def foo(x: String) = x + good

class A:
  @newS("good", false) @add("helloString") @hello
  val info = "A:"

  @add("info") @add("good")
  def foo(x: String) = x

@main def Test: Unit =
  val a = new A
  assert(a.info == "A:helloworld")
  assert(a.foo("good") == "goodgoodA:helloworld")
