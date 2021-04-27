package enums

def enumOrdinal[T](t: T): Int = t match
  case t: reflect.Enum => t.ordinal
  case t               => -1
