//> using options -feature

import language.experimental.into


object x1:
  def f1(x: List[into Int]) = () // error // error
object x3:
  def f3(x: ((into Int))) = () // ok
object x4:
  def f4(x: into Int*) = () // error
object x5:
  def f5(x: ((into Int))*) = () // ok

object x6:
  def f6(x: (into Int)*) = () // ok
  def f7(x: (Int => into Int)*) = () // ok
  def f8(x: (Int => (into Int))*) = () // ok
  def f9(x: (y: Int) => into Int) = () // ok
  def f10(x: ((y: Int) => into Int)*) = () // ok
  def f11(x: ((y: into Int) => into Int => Int)*) = () // error // error

object x7:
  def f14(x: (into Int) => Int) = () // error
  def f15(x: (into Int, into Int)) = () // error // error
  def f16(x: (into Int, into Int) => Int) = () // error // error
  def f17(x: into (y: into Int, z: into Int) => into Int) = () // error // error // error
