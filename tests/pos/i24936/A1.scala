// https://github.com/scala/scala3/issues/24936
package example

trait A1 { self: O1.type =>
  import O2.someKey
  def taskImpl: Unit = println(someKey)
}
