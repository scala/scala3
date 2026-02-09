package tests
package deprecated

class A:
  def defInt: Int
    = 1
  @deprecated(message = "1")
  def def1: 1
    = 1
  @deprecated("reason")
  val valInt: Int
    = 1
  val val1: 1
    = 1
  var varInt: Int
    = 1
  var var1: 1
    = 1
  class InnerA:
    val innerVal: Int
      = 1

class B extends A:
  @deprecated(since = "1", message = "some reason")
  def x: Int
    = 1
  val y: Int
    = 1

class C:
  /** zero */
  @deprecated
  def noInfo: Int
    = 0
  /** one */
  @deprecated()
  def noInfo2: Int
    = 0
  /** two */
  @deprecated("without names", "2.10.0")
  def noNames: Int
    = 0
  /** three */
  @deprecated(message = "with names", since = "2.10.0")
  def withNames: Int
    = 1
  /** four */
  @deprecated(since = "2.10.0", message = "backwards names")
  def backwardNames: Int
    = 2
  /** five */
  @deprecated("only message")
  def onlyUnnamedMessage: Int
    = 0
  /** six */
  @deprecated(message = "only named message")
  def onlyNamedMessage: Int
    = 1
  /** seven */
  @deprecated(since = "2.10.0")
  def onlyNamedSince: Int
    = 2