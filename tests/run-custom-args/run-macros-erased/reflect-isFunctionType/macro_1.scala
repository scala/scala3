import scala.quoted._


inline def isFunctionType[T]: Boolean = ${ isFunctionTypeImpl('[T]) }

def isFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.asTypeTree.tpe.isFunctionType)
}


inline def isContextFunctionType[T]: Boolean = ${ isContextFunctionTypeImpl('[T]) }

def isContextFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.asTypeTree.tpe.isContextFunctionType)
}


inline def isErasedFunctionType[T]: Boolean = ${ isErasedFunctionTypeImpl('[T]) }

def isErasedFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.asTypeTree.tpe.isErasedFunctionType)
}

inline def isDependentFunctionType[T]: Boolean = ${ isDependentFunctionTypeImpl('[T]) }

def isDependentFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty._
  Expr(tp.asTypeTree.tpe.isDependentFunctionType)
}

