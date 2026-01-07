package dotty.tools
package vulpix

enum Status:
  case Success(output: String)
  case Failure(output: String)
  case Timeout

  def isSuccess: Boolean = this.isInstanceOf[Success]
