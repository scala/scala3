import scala.quoted._


inline def isFunctionType[T]: Boolean = ${ isFunctionTypeImpl('[T]) }

def isFunctionTypeImpl[T](tp: Staged[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isFunctionType)
}


inline def isContextFunctionType[T]: Boolean = ${ isContextFunctionTypeImpl('[T]) }

def isContextFunctionTypeImpl[T](tp: Staged[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isContextFunctionType)
}


inline def isErasedFunctionType[T]: Boolean = ${ isErasedFunctionTypeImpl('[T]) }

def isErasedFunctionTypeImpl[T](tp: Staged[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isErasedFunctionType)
}

inline def isDependentFunctionType[T]: Boolean = ${ isDependentFunctionTypeImpl('[T]) }

def isDependentFunctionTypeImpl[T](tp: Staged[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isDependentFunctionType)
}

