import scala.quoted._


inline def isFunctionType[T:TypeTag]: Boolean = ${ isFunctionTypeImpl('[T]) }

def isFunctionTypeImpl[T](tp: TypeTag[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty.{_, given}
  Expr(tp.unseal.tpe.isFunctionType)
}


inline def isImplicitFunctionType[T:TypeTag]: Boolean = ${ isImplicitFunctionTypeImpl('[T]) }

def isImplicitFunctionTypeImpl[T](tp: TypeTag[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty.{_, given}
  Expr(tp.unseal.tpe.isImplicitFunctionType)
}


inline def isErasedFunctionType[T:TypeTag]: Boolean = ${ isErasedFunctionTypeImpl('[T]) }

def isErasedFunctionTypeImpl[T](tp: TypeTag[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty.{_, given}
  Expr(tp.unseal.tpe.isErasedFunctionType)
}

inline def isDependentFunctionType[T:TypeTag]: Boolean = ${ isDependentFunctionTypeImpl('[T]) }

def isDependentFunctionTypeImpl[T](tp: TypeTag[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty.{_, given}
  Expr(tp.unseal.tpe.isDependentFunctionType)
}

