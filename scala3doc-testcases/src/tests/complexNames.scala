package tests

package complexNames

abstract class A:
  def ++(other: A): A
  def +:(other: Int): A
  def :+(other: Int): A
  // scala3doc has problems with names in backticks
  // def `multi word name`: Int
  // def `*** name with arbitrary chars ^%`: Int
  def complexName_^*(param: String): A
