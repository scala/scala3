package dotty.dokka.tasty

import scala.tasty.reflect._
import org.jetbrains.dokka.model.doc._

trait ScaladocSupport { self: TastyParser =>
    def parseComment(comment: reflect.Comment, tree: reflect.Tree): DocumentationNode =
        inspector.parser.parse(comment.raw) // TODO use scaladoc reader to generate proper docs
}