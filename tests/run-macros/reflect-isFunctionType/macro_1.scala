import scala.quoted._
import scala.tasty._


inline def isFunctionType[T:Type]: Boolean = ${ isFunctionTypeImpl('[T]) }

def isFunctionTypeImpl[T](tp: Type[T])(implicit refl: Reflection): Expr[Boolean] = {
  import refl._
  tp.unseal.tpe.isFunctionType.toExpr
}


inline def isImplicitFunctionType[T:Type]: Boolean = ${ isImplicitFunctionTypeImpl('[T]) }

def isImplicitFunctionTypeImpl[T](tp: Type[T])(implicit refl: Reflection): Expr[Boolean] = {
  import refl._
  tp.unseal.tpe.isImplicitFunctionType.toExpr
}


inline def isErasedFunctionType[T:Type]: Boolean = ${ isErasedFunctionTypeImpl('[T]) }

def isErasedFunctionTypeImpl[T](tp: Type[T])(implicit refl: Reflection): Expr[Boolean] = {
  import refl._
  tp.unseal.tpe.isErasedFunctionType.toExpr
}

inline def isDependentFunctionType[T:Type]: Boolean = ${ isDependentFunctionTypeImpl('[T]) }

def isDependentFunctionTypeImpl[T](tp: Type[T])(implicit refl: Reflection): Expr[Boolean] = {
  import refl._
  tp.unseal.tpe.isDependentFunctionType.toExpr
}

