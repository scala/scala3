import scala.quoted._


inline def isFunctionType[T]: Boolean = ${ isFunctionTypeImpl(Type[T]) }

def isFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isFunctionType)
}


inline def isContextFunctionType[T]: Boolean = ${ isContextFunctionTypeImpl(Type[T]) }

def isContextFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isContextFunctionType)
}


inline def isErasedFunctionType[T]: Boolean = ${ isErasedFunctionTypeImpl(Type[T]) }

def isErasedFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isErasedFunctionType)
}

inline def isDependentFunctionType[T]: Boolean = ${ isDependentFunctionTypeImpl(Type[T]) }

def isDependentFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.unseal.tpe.isDependentFunctionType)
}

