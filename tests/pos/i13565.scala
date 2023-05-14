package ips.clang

sealed trait         PrimaryExpression extends         PostfixExpression
sealed trait         PostfixExpression extends           UnaryExpression
sealed trait           UnaryExpression extends            CastExpression
sealed trait            CastExpression extends  MultiplicativeExpression
sealed trait  MultiplicativeExpression extends        AdditiveExpression
sealed trait        AdditiveExpression extends           ShiftExpression
sealed trait           ShiftExpression extends      RelationalExpression
sealed trait      RelationalExpression extends        EqualityExpression
sealed trait        EqualityExpression extends         PrecAndExpression
sealed trait         PrecAndExpression extends PrecExclusiveOrExpression
sealed trait PrecExclusiveOrExpression extends PrecInclusiveOrExpression
sealed trait PrecInclusiveOrExpression extends  PrecLogicalAndExpression
sealed trait  PrecLogicalAndExpression extends   PrecLogicalOrExpression
sealed trait   PrecLogicalOrExpression extends PrecConditionalExpression
sealed trait PrecConditionalExpression extends   PrecAssigmentExpression
sealed trait   PrecAssigmentExpression extends                Expression
sealed trait                Expression
sealed trait          BinaryExpression { def op: String; def frs: Expression; def snd: Expression }

type TypeName = String
case class                     Identifier(value: String)                                                                  extends         PrimaryExpression
case class                    IntConstant(value: Int)                                                                     extends         PrimaryExpression
case class                   CharConstant(value: Char)                                                                    extends         PrimaryExpression
case class                  StringLiteral(value: String)                                                                  extends         PrimaryExpression
case class              WrappedExpression(value: Expression)                                                              extends         PrimaryExpression
case class           ArrayIndexExpression(base: PostfixExpression, index: Expression)                                     extends         PostfixExpression
case class         FunctionCallExpression(fun: PostfixExpression, arguments: List[PrecAssigmentExpression])               extends         PostfixExpression
case class            DotSelectExpression(qualifier: PostfixExpression, select: Identifier)                               extends         PostfixExpression
case class          ArrowSelectExpression(qualifier: PostfixExpression, select: Identifier)                               extends         PostfixExpression
case class     PostfixIncrementExpression(base: PostfixExpression)                                                        extends         PostfixExpression
case class     PostfixDecrementExpression(base: PostfixExpression)                                                        extends         PostfixExpression
case class                CompoundLiteral(typeName: TypeName, initializers: List[Int])                                    extends         PostfixExpression
case class      PrefixIncrementExpression(base: UnaryExpression)                                                          extends           UnaryExpression
case class      PrefixDecrementExpression(base: UnaryExpression)                                                          extends           UnaryExpression
case class        UnaryOperatorExpression(op: String, argument: CastExpression)                                           extends           UnaryExpression
case class          SizeofConstExpression(expression: UnaryExpression)                                                    extends           UnaryExpression
case class           SizeofTypeExpression(typeName: TypeName)                                                             extends           UnaryExpression
case class                           Cast(typeName: TypeName, argument: CastExpression)                                   extends            CastExpression
case class MultiplicativeBinaryExpression(op: String, frs:      MultiplicativeExpression, snd:            CastExpression) extends  MultiplicativeExpression with BinaryExpression
case class       AdditiveBinaryExpression(op: String, frs:      MultiplicativeExpression, snd:            CastExpression) extends  MultiplicativeExpression with BinaryExpression
case class          ShiftBinaryExpression(op: String, frs:      MultiplicativeExpression, snd:            CastExpression) extends  MultiplicativeExpression with BinaryExpression
case class     RelationalBinaryExpression(op: String, frs:          RelationalExpression, snd:           ShiftExpression) extends      RelationalExpression with BinaryExpression
case class       EqualityBinaryExpression(op: String, frs:          RelationalExpression, snd:           ShiftExpression) extends        EqualityExpression with BinaryExpression
case class            AndBinaryExpression(op: String, frs:             PrecAndExpression, snd:        EqualityExpression) extends         PrecAndExpression with BinaryExpression
case class    ExclusiveOrBinaryExpression(op: String, frs:     PrecExclusiveOrExpression, snd:         PrecAndExpression) extends PrecExclusiveOrExpression with BinaryExpression
case class    InclusiveOrBinaryExpression(op: String, frs:     PrecExclusiveOrExpression, snd:         PrecAndExpression) extends PrecInclusiveOrExpression with BinaryExpression
case class     LogicalAndBinaryExpression(op: String, frs:      PrecLogicalAndExpression, snd: PrecInclusiveOrExpression) extends  PrecLogicalAndExpression with BinaryExpression
case class      LogicalOrBinaryExpression(op: String, frs:      PrecLogicalAndExpression, snd: PrecInclusiveOrExpression) extends   PrecLogicalOrExpression with BinaryExpression
case class          ConditionalExpression(cond: PrecLogicalOrExpression, frs: Expression, snd: PrecConditionalExpression) extends PrecConditionalExpression
case class            AssigmentExpression(op: String, frs:               UnaryExpression, snd:   PrecAssigmentExpression) extends   PrecAssigmentExpression
case class                CommaExpression(frs: Expression, snd: Expression)                                               extends                Expression
case class             AltCommaExpression(frs: Expression, snd: Expression)                                               extends                Expression

// each AdditionalUnaryExpressionX increase compilation time
sealed trait AdditionalUnaryExpression1 extends UnaryExpression
sealed trait AdditionalUnaryExpression2 extends UnaryExpression
sealed trait AdditionalUnaryExpression3 extends UnaryExpression
sealed trait AdditionalUnaryExpression4 extends UnaryExpression
sealed trait AdditionalUnaryExpression5 extends UnaryExpression

class LongCompilation:
  // this match used to take 2m30 to compile, with 1 cache it now takes 5s (30x speedup, aka 3000%)
  def toCastExpression(expr: Expression): CastExpression = expr match
    case x: CastExpression => x
    case _                 => WrappedExpression(expr)
