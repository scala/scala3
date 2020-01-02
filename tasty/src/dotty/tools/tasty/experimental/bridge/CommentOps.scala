package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait CommentOps extends Core with

  given CommentOps: (comment: Comment) extended with
    def raw: String = internal.Comment_raw(comment)
    def span: Span = internal.Comment_span(comment)
