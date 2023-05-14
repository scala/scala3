type IsString[T <: Any] = T match {
  case String => true
  case _ => false
}

def isString(x: Any): IsString[x.type] = x match
  case _: String => true
  case _ => false

def isString2(x: Any): x.type match {
  case String => true
  case _ => false
} = x match
  case _: String => true
  case _ => false

def isString3[T](x: T): T match {
  case String => true
  case _ => false
} = x match
  case _: String => true
  case _ => false
