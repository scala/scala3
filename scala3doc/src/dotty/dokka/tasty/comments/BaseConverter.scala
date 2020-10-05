package dotty.dokka.tasty.comments

import scala.jdk.CollectionConverters._

import org.jetbrains.dokka.model.{doc => dkkd}

/** Quick'n'dirty class to remove some code duplication */
trait BaseConverter {

  protected def withParsedQuery(queryStr: String)(thunk: Query => dkkd.DocTag): dkkd.DocTag = {
    QueryParser(queryStr).tryReadQuery() match {
      case Left(err) =>
        val msg = err.getMessage
        // TODO: for better experience we should show source location here
        println("WARN: " + msg)
        dkkd.A(List(dkk.text(queryStr)).asJava, Map("href" -> "#").asJava)
      case Right(query) =>
        thunk(query)
    }
  }

  protected val SchemeUri = """[a-z]+:.*""".r
}
