package tests

package complexNames

abstract class A:
  def ++(other: A): A
  def +:(other: Int): A
  def :+(other: Int): A

  // scaladoc has problems with names in backticks
  // def `multi word name`: Int
  // def `*** name with arbitrary chars ^%`: Int
  // def `mischievous(param:Int)`(otherParam: Int): String
  // def withMischievousParams(`param: String, param2`: String): String

  def complexName_^*(param: String): A

  def `completelyUnnecessaryBackticks`: Int //expected: def completelyUnnecessaryBackticks: Int
  def `+++:`(other: Int): A //expected: def +++:(other: Int): A
  def `:+++`(other: Int): A //expected: def :+++(other: Int): A

  def `abc_^^_&&`: A //expected: def abc_^^_&&: A
  def `abc_def`: A //expected: def abc_def: A
  def `abc_def_++`: A //expected: def abc_def_++: A
  // def `++_abc`: A
  // def `abc_++_--`: A
