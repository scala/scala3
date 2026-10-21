// https://github.com/scala/scala3/issues/25801
package i25801

trait A { self: B =>
  type X
}

trait B extends A {
  type X
}
