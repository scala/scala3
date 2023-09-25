import scala.quoted.*
inline def generateCode: Unit = ${ testLocalDummyOwner }
def testLocalDummyOwner(using Quotes): Expr[Unit] = '{ trait E { $valInBlock } }
def valInBlock(using Quotes): Expr[Unit] = '{ val x: Int = 2; () }
