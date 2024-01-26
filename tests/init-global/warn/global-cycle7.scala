object A {
  val n: Int = B.m
}

object B {
  val m: Int = A.n
}

abstract class TokensCommon {
  def maxToken: Int

  val tokenString, debugString: Array[String] = new Array[String](maxToken + 1)
}

object JavaTokens extends TokensCommon {
  final def maxToken: Int = DOUBLE
  final val DOUBLE = 188
}

