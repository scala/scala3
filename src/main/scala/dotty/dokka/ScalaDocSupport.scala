package dotty.dokka

import scala.tasty.reflect._
import org.jetbrains.dokka.model.doc._

trait ScaladocSupport { self: DottyDokkaParser =>
    def parseComment(commment: reflect.Comment, tree: reflect.Tree): DocumentationNode =
        parser.parse(comment.raw) // TODO use scaladoc reader to generate proper docs
}