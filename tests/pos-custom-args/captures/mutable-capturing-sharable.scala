import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*
class Logger extends SharedCapability:
  def log(msg: String): Unit = ()
class TracedRef(logger: Logger^{any.only[SharedCapability]}) extends Stateful:
  private var _data: Int = 0
  update def set(newValue: Int): Unit =
    logger.log("set")
    _data = newValue
  def get: Int =
    logger.log("get")  // error, but should it be allowed?
    _data

class TracedRef2(logger: Logger) extends Stateful:
  private var _data: Int = 0
  update def set(newValue: Int): Unit =
    logger.log("set")
    _data = newValue
  def get: Int =
    logger.log("get")  // error, but should it be allowed?
    _data
