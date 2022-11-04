// When compiling this with -Yprofile, this should output
//
// Source file         Lines   Tokens    Tasty  Complexity/Line     Directory
// profile-test.scala     16       50      316   0.49  low          tests/pos
object ProfileTest:

  def test = ???

  def bar: Boolean = ??? ;
  def baz = ???

  /** doc comment
   */
  def bam = ???

  if bar then
    // comment
    baz
  else
    bam

  if bar then {
    baz
  }
  else {
    // comment
    bam
  }

