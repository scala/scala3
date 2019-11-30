package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait AnnotationOps extends Core with

  object Annotation with

    object Child with

      def unapply(annot: Annotation)(given Context): Option[Symbol] = internal.Annotation_Child_unapply(annot)

  given AnnotationOps: (annot: Annotation) with
    def tree(given Context): Tree = internal.Annotation_tree(annot)
    def symbol(given Context): Symbol = internal.Annotation_symbol(annot)
