package dotty.tools.tasty.experimental

import bridge._
import reflect.ClassTag

trait Tasty extends Core
  with NameOps
  with SignatureOps
  with TreeOps
  with ConstantOps
  with ContextOps
  with TypeOps
  with SymbolOps
  with StringOps
  with PrintingOps
  with AnnotationOps
  with FlagOps
  with PositionOps
