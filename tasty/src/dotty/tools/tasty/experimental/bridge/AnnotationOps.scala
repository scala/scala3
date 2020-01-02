package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait AnnotationOps extends Core with
  self: TreeOps with PositionOps with ContextOps with SourceFileOps with SymbolOps =>

  given ClassTag[Annotation] = internal.Annotation_CT

  object Annotation with

    object Child with

      def unapply(annot: Annotation)(given Context): Option[Symbol] = internal.Annotation_Child_unapply(annot)

  given AnnotationOps: (annot: Annotation) extended with
    def tree(given Context): tpd.Tree = internal.Annotation_tree(annot)
    def symbol(given Context): Symbol = internal.Annotation_symbol(annot)
