object Test {
  println (Object.reflect.runtime.universe.reify (new Object().getClass)) // error: not found Object
}
