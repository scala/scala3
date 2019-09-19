import scala.quoted._


inline def isFunctionType[T:Type]: Boolean = ${ isFunctionTypeImpl('[T]) }

def isFunctionTypeImpl[T](tp: Type[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isFunctionType)
}


inline def isImplicitFunctionType[T:Type]: Boolean = ${ isImplicitFunctionTypeImpl('[T]) }

def isImplicitFunctionTypeImpl[T](tp: Type[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isImplicitFunctionType)
}


inline def isErasedFunctionType[T:Type]: Boolean = ${ isErasedFunctionTypeImpl('[T]) }

def isErasedFunctionTypeImpl[T](tp: Type[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isErasedFunctionType)
}

inline def isDependentFunctionType[T:Type]: Boolean = ${ isDependentFunctionTypeImpl('[T]) }

def isDependentFunctionTypeImpl[T](tp: Type[T])(given qctx: QuoteContext): Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isDependentFunctionType)
}

