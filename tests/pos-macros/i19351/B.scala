import scala.quoted.*
// import A.*
def myMacroExpr(using Quotes): Expr[x.type] = '{???}