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

class InkuireDelegateSearchEngine {

  given ec: ExecutionContext = global

  val ec2 = "https://pmfyy2t0sc.execute-api.eu-central-1.amazonaws.com/prod" //TODO configure

  private def getURLContent(url: String): Future[String] = Ajax.get(url).map(_.responseText).fallbackTo(Future("[]"))

  def dynamicToPageEntry(d: Dynamic): PageEntry =
    PageEntry(
      d.functionName.asInstanceOf[String],
      d.prettifiedSignature.asInstanceOf[String],
      d.pageLocation.asInstanceOf[String],
      d.functionName.asInstanceOf[String],
      List.empty
    )

  def query(s: String)(callback: PageEntry => Node)(errorCallback: String => Node): Unit = { //TODO handle errors
    val signature = URIUtils.encodeURIComponent(s)
    val request = getURLContent(ec2 + "/forSignature?signature=" + signature)
    request.foreach { (s: String) =>
      JSON.parse(s).matches.asInstanceOf[js.Array[Dynamic]].map(dynamicToPageEntry).foreach(callback)
    }
  }

}
