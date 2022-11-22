package dotty.tools
package dotc
package parsing

import collection.immutable.BitSet

object JavaTokens extends TokensCommon {
  inline val minToken = EMPTY
  final def maxToken: Int = DOUBLE

  final val javaOnlyKeywords: TokenSet = tokenRange(INSTANCEOF, ASSERT)
  final val sharedKeywords: BitSet = BitSet( IF, FOR, ELSE, THIS, NULL, NEW, SUPER, ABSTRACT, FINAL, PRIVATE, PROTECTED,
    EXTENDS, TRUE, FALSE, CLASS, IMPORT, PACKAGE, DO, THROW, TRY, CATCH, FINALLY, WHILE, RETURN )
  final val primTypes: TokenSet = tokenRange(VOID, DOUBLE)
  final val keywords: BitSet = sharedKeywords | javaOnlyKeywords | primTypes

  /** keywords */
  inline val INSTANCEOF = 101;       enter(INSTANCEOF, "instanceof")
  inline val CONST = 102;            enter(CONST, "const")

  /** templates */
  inline val INTERFACE = 105;        enter(INTERFACE, "interface")
  inline val ENUM = 106;             enter(ENUM, "enum")
  inline val IMPLEMENTS = 107;       enter(IMPLEMENTS, "implements")

  /** modifiers */
  inline val PUBLIC = 110;           enter(PUBLIC, "public")
  inline val DEFAULT = 111;          enter(DEFAULT, "default")
  inline val STATIC = 112;           enter(STATIC, "static")
  inline val TRANSIENT = 113;        enter(TRANSIENT, "transient")
  inline val VOLATILE = 114;         enter(VOLATILE, "volatile")
  inline val SYNCHRONIZED = 115;     enter(SYNCHRONIZED, "synchronized")
  inline val NATIVE = 116;           enter(NATIVE, "native")
  inline val STRICTFP = 117;         enter(STRICTFP, "strictfp")
  inline val THROWS = 118;           enter(THROWS, "throws")

  /** control structures */
  inline val BREAK = 130;            enter(BREAK, "break")
  inline val CONTINUE = 131;         enter(CONTINUE, "continue")
  inline val GOTO = 132;             enter(GOTO, "goto")
  inline val SWITCH = 133;           enter(SWITCH, "switch")
  inline val ASSERT = 134;           enter(ASSERT, "assert")

  /** special symbols */
  inline val EQEQ = 140
  inline val BANGEQ = 141
  inline val LT = 142
  inline val GT = 143
  inline val LTEQ = 144
  inline val GTEQ = 145
  inline val BANG = 146
  inline val QMARK = 147
  inline val AMP = 148
  inline val BAR = 149
  inline val PLUS = 150
  inline val MINUS = 151
  inline val ASTERISK = 152
  inline val SLASH = 153
  inline val PERCENT = 154
  inline val HAT = 155
  inline val LTLT = 156
  inline val GTGT = 157
  inline val GTGTGT = 158
  inline val AMPAMP = 159
  inline val BARBAR = 160
  inline val PLUSPLUS = 161
  inline val MINUSMINUS = 162
  inline val TILDE = 163
  inline val DOTDOTDOT = 164
  inline val AMPEQ = 165
  inline val BAREQ = 166
  inline val PLUSEQ = 167
  inline val MINUSEQ = 168
  inline val ASTERISKEQ = 169
  inline val SLASHEQ = 170
  inline val PERCENTEQ = 171
  inline val HATEQ = 172
  inline val LTLTEQ = 173
  inline val GTGTEQ = 174
  inline val GTGTGTEQ = 175

  /** primitive types */
  inline val VOID = 180;             enter(VOID, "void")
  inline val BOOLEAN = 181;          enter(BOOLEAN, "boolean")
  inline val BYTE = 182;             enter(BYTE, "byte")
  inline val SHORT = 183;            enter(SHORT, "short")
  inline val CHAR = 184;             enter(CHAR, "char")
  inline val INT = 185;              enter(INT, "int")
  inline val LONG = 186;             enter(LONG, "long")
  inline val FLOAT = 187;            enter(FLOAT, "float")
  inline val DOUBLE = 188;           enter(DOUBLE, "double")
}
