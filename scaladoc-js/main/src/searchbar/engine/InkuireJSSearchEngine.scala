package dotty.tools.scaladoc

import scala.io.Source
import dotty.tools.scaladoc.PageEntry
import org.scalajs.dom.webworkers.Worker
import org.scalajs.dom._
import scala.scalajs.js.{ JSON, Dynamic }
import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.scalajs.js.timers._
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js.URIUtils

class InkuireJSSearchEngine {

  val scriptPath     = Globals.pathToRoot + "scripts/"
  val worker: Worker = new Worker(scriptPath + "inkuire-worker.js")

  def dynamicToMatch(d: Dynamic): InkuireMatch = {
    InkuireMatch(
      d.prettifiedSignature.asInstanceOf[String],
      d.functionName.asInstanceOf[String],
      d.packageLocation.asInstanceOf[String],
      d.pageLocation.asInstanceOf[String],
      d.entryType.asInstanceOf[String],
      d.mq.asInstanceOf[Int]
    )
  }

  def query(s: String)(callback: InkuireMatch => Unit)(endCallback: String => Unit): Unit = {
    worker.onmessage = _ => ()
    val func = (msg: MessageEvent) => {
      msg.data.asInstanceOf[String] match {
        case "engine_ready" =>
        case "new_query" =>
        case endMsg if endMsg.startsWith("query_ended") =>
          endCallback(endMsg.drop("query_ended".length))
        case q =>
          val matches = JSON.parse(q).matches
          val actualMatches = matches.asInstanceOf[js.Array[Dynamic]].map(dynamicToMatch)
          actualMatches.foreach(callback)
      }
    }
    worker.onmessage = func
    worker.postMessage(s)
  }

}