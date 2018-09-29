object Test {

  implicit object O {
    def l1(this x: String) = x.length
    def l1(x: Int) = x * x
    def l2(x: String) = x.length
  }

  "".l1 // OK
  "".l2 // error
  1.l1 // error


}