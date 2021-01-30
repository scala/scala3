import scala.quoted.*


inline def isFunctionType[T]: Boolean = ${ isFunctionTypeImpl[T] }

def isFunctionTypeImpl[T: Type](using Quotes) : Expr[Boolean] = {
  import quotes.reflect.*
  Expr(TypeRepr.of[T].isFunctionType)
}


inline def isContextFunctionType[T]: Boolean = ${ isContextFunctionTypeImpl[T] }

def isContextFunctionTypeImpl[T: Type](using Quotes) : Expr[Boolean] = {
  import quotes.reflect.*
  Expr(TypeRepr.of[T].isContextFunctionType)
}


inline def isErasedFunctionType[T]: Boolean = ${ isErasedFunctionTypeImpl[T] }

def isErasedFunctionTypeImpl[T: Type](using Quotes) : Expr[Boolean] = {
  import quotes.reflect.*
  Expr(TypeRepr.of[T].isErasedFunctionType)
}

inline def isDependentFunctionType[T]: Boolean = ${ isDependentFunctionTypeImpl[T] }

def isDependentFunctionTypeImpl[T: Type](using Quotes) : Expr[Boolean] = {
  import quotes.reflect.*
  Expr(TypeRepr.of[T].isDependentFunctionType)
}

