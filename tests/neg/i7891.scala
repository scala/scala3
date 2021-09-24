// was previously ok in one compilation unit
def f22 = "hello, world"

package p {
  @main def m = println(f22)  // error
}
