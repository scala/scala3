import scala.quoted.*
transparent inline def getTypeInfo[T]() = ${ getTypeInfoImpl[T] }
def getTypeInfoImpl[T: Type](using ctx: Quotes): Expr[Unit] = '{ () }
