class MyFunction(arg: String) {
  def a: String = arg
}

trait MyFun[+R] extends MyFunction {
  def apply(i: Int): R
}

val myFun: MyFun[Int] = (i: Int) => 1 // error

//

class MyFunction1(arg: String = "") {
  def a: String = arg
}

trait MyFun1[+R] extends MyFunction1 {
  def apply(i: Int): R
}

val myFun1: MyFun1[Int] = (i: Int) => 1 // error

//

trait MyFunction2(arg: String = "") {
  def a: String = arg
}

trait MyFun2[+R] extends MyFunction2 {
  def apply(i: Int): R
}

val myFun2: MyFun2[Int] = (i: Int) => 1

//

trait MyFunction3(arg: String) {
  def a: String = arg
}

trait MyFun3[+R] extends MyFunction3 {
  def apply(i: Int): R
}

val myFun3: MyFun3[Int] = (i: Int) => 1

//

class MyFunction4() {
}

trait MyFun4[+R] extends MyFunction4 {
  def apply(i: Int): R
}

val myFun4: MyFun4[Int] = (i: Int) => 1
