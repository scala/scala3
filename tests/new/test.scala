
package foo

package object bar:
  opaque type O[X] >: X = X

class Test:
  import bar.O

  val x = "abc"
  val y: O[String] = x
  //val z: String = y



