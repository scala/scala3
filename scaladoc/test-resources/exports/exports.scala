package pkg

class Bass:
  /** parameterless method 1 */
  def parameterless1: Int = 1
  /** parameterless method 2 */
  def parameterless2: Int = 2

  /** method with empty parameter list 1 */
  def emptyParameterList1(): Int = 1
  /** method with empty parameter list 2 */
  def emptyParameterList2(): Int = 2

  /** method with one parameter 1 */
  def oneParameter1(x: Int): Int = 1
  /** method with one parameter 2 */
  def oneParameter2(x: Int): Int = 2

class Midrange(val bass: Bass):
  // scaladoc only shows "parameterless1" as a defined export
  export bass.{parameterless1, emptyParameterList1, oneParameter1}

class Treble(x: Bass) extends Midrange(x):
  // scaladoc only shows "parameterless2" as a defined export
  // and "parameterless1" as an inherited defined export
  export bass.{parameterless2, emptyParameterList2, oneParameter2}