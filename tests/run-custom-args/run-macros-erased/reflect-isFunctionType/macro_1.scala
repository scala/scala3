import scala.quoted._


inline def isFunctionType[T]: Boolean = ${ isFunctionTypeImpl('[T]) }

def isFunctionTypeImpl[T](using s: Scope)(tp: s.Type[T]): s.Expr[Boolean] =
  Expr(tp.tpe.isFunctionType)


inline def isContextFunctionType[T]: Boolean = ${ isContextFunctionTypeImpl('[T]) }

def isContextFunctionTypeImpl[T](using s: Scope)(tp: s.Type[T]): s.Expr[Boolean] =
  Expr(tp.tpe.isContextFunctionType)


inline def isErasedFunctionType[T]: Boolean = ${ isErasedFunctionTypeImpl('[T]) }

def isErasedFunctionTypeImpl[T](using s: Scope)(tp: s.Type[T]): s.Expr[Boolean] =
  Expr(tp.tpe.isErasedFunctionType)


inline def isDependentFunctionType[T]: Boolean = ${ isDependentFunctionTypeImpl('[T]) }

def isDependentFunctionTypeImpl[T](using s: Scope)(tp: s.Type[T]): s.Expr[Boolean] =
  Expr(tp.tpe.isDependentFunctionType)
