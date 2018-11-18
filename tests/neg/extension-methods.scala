object Test {

  implicit object O {
    def (x: String) l1 = x.length
    def l1(x: Int) = x * x
    def l2(x: String) = x.length
  }

  "".l1 // OK
  "".l2 // error
  1.l1 // error


}