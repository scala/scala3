import scala.quoted.*

import reflect.Selectable.reflectiveSelectable

inline def testShowErrType: Unit = ${ testShowErrTypeImpl }

def testShowErrTypeImpl(using q: Quotes): Expr[Unit] = {
  import q.reflect.*

  type Types = {
    def UnspecifiedErrorType: Any
  }

  val t: Types = Class.forName("dotty.tools.dotc.core.Types$").getField("MODULE$").get(null).asInstanceOf[Types]

  val tpe = t.UnspecifiedErrorType.asInstanceOf[TypeRepr]

  val repr = tpe.show

  '{ println(${Expr(repr)}) }
}
