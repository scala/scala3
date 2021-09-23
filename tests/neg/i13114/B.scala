class D2 extends C

package p {
  class D extends C  // error: not found

  @main def test = println(new D)
}
