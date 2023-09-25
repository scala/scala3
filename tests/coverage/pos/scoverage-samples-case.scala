// minimized example from sbt-scoverage-samples
package org.scoverage.samples

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case object StartService
case object StopService

class PriceEngine() {

  var cancellable: String = _

  cancellable = "abc"

  def receive: Any => Unit = {
    case StartService =>
      stop()

    case StopService =>
      stop()
  }

  def stop(): Unit = {
    if (cancellable != null)
      println("stop")
  }
}
