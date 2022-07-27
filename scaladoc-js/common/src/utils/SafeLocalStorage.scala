package dotty.tools.scaladoc

import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import scala.util.chaining._

class SafeLocalStorage[T <: js.Any](key: String, defaultValue: T) {

  val isLocalStorageSupported: Boolean = try {
    val testKey = "__TEST__KEY__"
    window.localStorage.setItem(testKey, "")
    window.localStorage.removeItem(testKey)
    true
  } catch {
    case _ => false
  }

  def checkSupport[U](defaultValue: U)(callback: () => U): U =
    if isLocalStorageSupported then callback() else defaultValue

  private def parseData(data: String): T =
    try {
      Option(JSON.parse(data).asInstanceOf[T]).getOrElse(defaultValue)
    } catch {
      case _ => defaultValue
    }

  def getData: T =
    checkSupport(defaultValue) { () =>
      window.localStorage
        .getItem(key)
        .pipe(parseData)
    }

  def setData(data: T): Unit =
    checkSupport(()) { () =>
      JSON.stringify(data)
        .pipe(window.localStorage.setItem(key, _))
    }

  def isEmpty: Boolean = getData == defaultValue
}
