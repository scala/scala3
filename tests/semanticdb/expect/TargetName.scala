package example

object TargetName:
  @annotation.targetName("m1")
  def m(i: Int) = 1
  @annotation.targetName("m2")
  def m(i: Int) = 1
  def m1(i: String) = 1
