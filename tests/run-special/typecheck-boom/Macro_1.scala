import scala.quoted.*

inline def boom: Int = ${ boomImpl }

def boomImpl(using Quotes): Expr[Int] = throw new RuntimeException("boom")
