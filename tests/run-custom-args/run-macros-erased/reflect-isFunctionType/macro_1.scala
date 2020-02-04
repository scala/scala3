import scala.quoted._


inline def isFunctionType[T:Type]: Boolean = ${ isFunctionTypeImpl('[T]) }

def isFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty.{_, given _}
  Expr(tp.unseal.tpe.isFunctionType)
}


inline def isContextFunctionType[T:Type]: Boolean = ${ isContextFunctionTypeImpl('[T]) }

def isContextFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty.{_, given _}
  Expr(tp.unseal.tpe.isContextFunctionType)
}


inline def isErasedFunctionType[T:Type]: Boolean = ${ isErasedFunctionTypeImpl('[T]) }

def isErasedFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty.{_, given _}
  Expr(tp.unseal.tpe.isErasedFunctionType)
}

inline def isDependentFunctionType[T:Type]: Boolean = ${ isDependentFunctionTypeImpl('[T]) }

def isDependentFunctionTypeImpl[T](tp: Type[T])(using qctx: QuoteContext) : Expr[Boolean] = {
  import qctx.tasty.{_, given _}
  Expr(tp.unseal.tpe.isDependentFunctionType)
}

