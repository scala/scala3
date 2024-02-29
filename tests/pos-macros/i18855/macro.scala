import scala.quoted.*
import scala.language.experimental.captureChecking

def impl()(using Quotes): Expr[Unit] = '{()}
inline def run(): Unit = ${impl()}


