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

import scala.concurrent.ExecutionContext.global
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

case class Match(
  prettifiedSignature: String,
  functionName:        String,
  packageLocation:     String,
  pageLocation:        String
)

case class OutputFormat(
  query:   String,
  matches: List[Match]
)

//TODO CORS problems
class InkuireDelegateSearchEngine {

  given ec: ExecutionContext = global

  val ec2 = "http://ec2-52-59-255-148.eu-central-1.compute.amazonaws.com:8080" //TODO configure

  private def getURLContent(url: String): Future[String] = Ajax.get(url).map(_.responseText).fallbackTo(Future("[]"))
  
  def dynamicToPageEntry(d: Dynamic): PageEntry =
    PageEntry(
      d.functionName.asInstanceOf[String],
      d.prettifiedSignature.asInstanceOf[String],
      d.pageLocation.asInstanceOf[String],
      d.functionName.asInstanceOf[String],
      List.empty
    )

  def query(s: String)(callback: PageEntry => Node): Unit = {
    val signature = URIUtils.encodeURIComponent(s)
    getURLContent(ec2 + "/forSignature?signature=" + signature).map(JSON.parse(_)).foreach { (d: Dynamic) =>
      d.matches.asInstanceOf[js.Array[Dynamic]].map(dynamicToPageEntry).foreach(callback)
    }
  }

}

class InkuireJSSearchEngine {

  val scriptPath = Globals.pathToRoot + "scripts/"
  val worker     = new Worker(s"${scriptPath}inkuire-worker.js")

  def dynamicToPageEntry(d: Dynamic): PageEntry = {
    PageEntry(
      d.functionName.asInstanceOf[String],
      d.prettifiedSignature.asInstanceOf[String],
      d.pageLocation.asInstanceOf[String],
      d.functionName.asInstanceOf[String],
      List.empty
    )
  }

  def query(s: String)(callback: PageEntry => Node): List[PageEntry] = {
    worker.onmessage = _ => ()
    val res = ListBuffer[PageEntry]()
    val func = (msg: MessageEvent) => {
      msg.data.asInstanceOf[String] match {
        case "engine_ready" =>
        case "new_query" =>
        case q =>
          val matches = JSON.parse(q).matches
          val actualMatches = matches.asInstanceOf[js.Array[Dynamic]].map(dynamicToPageEntry)
          actualMatches.foreach(callback)
      }
    }
    worker.onmessage = func
    worker.postMessage(s)
    res.toList
  }

}
