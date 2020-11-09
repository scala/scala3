package tests

package complexNames

import scala.annotation.StaticAnnotation

class `*** Annotation` extends StaticAnnotation
class `OtherAnnotation` extends StaticAnnotation

class `*** Type`
class `OtherType`

abstract class A:
  def ++(other: A): A
  def +:(other: Int): A
  def :+(other: Int): A

  def `multi word name`: Int
  def `*** name with arbitrary chars ^%`: Int
  def `mischievous(param:Int)`(otherParam: Int): String
  def withMischievousParams(`param: String, param2`: String): String

  def complexName_^*(param: String): A

  def `completelyUnnecessaryBackticks`: Int //expected: def completelyUnnecessaryBackticks: Int
  def `+++:`(other: Int): A //expected: def +++:(other: Int): A
  def `:+++`(other: Int): A //expected: def :+++(other: Int): A

  def `abc_^^_&&`: A
  def `abc_def`: A //expected: def abc_def: A
  def `abc_def_++`: A //expected: def abc_def_++: A
  def `++_abc`: A
  def `abc_++_--`: A

  @`*** Annotation` def withStrangeAnnotation: A
  @`OtherAnnotation` def withOtherAnnotation: A //expected: @OtherAnnotation def withOtherAnnotation: A
  @OtherAnnotation def withOtherAnnotation2: A

  def withStrangeType: `*** Type`
  def withOtherType: `OtherType` //expected: def withOtherType: OtherType
  def withOtherType2: OtherType

  def `class`: A
  def `case`: A
  def `=>`: A
  def `=>:`(other: A): A //expected: def =>:(other: A): A
  def `caseclass`: A //expected: def caseclass: A
  def `Class`: A //expected: def Class: A
