package dotty.tools
package dotc
package parsing

import collection.immutable.BitSet

object JavaTokens extends TokensCommon {
  final val minToken = EMPTY
  final def maxToken = DOUBLE

  final val javaOnlyKeywords = tokenRange(INSTANCEOF, ASSERT)
  final val sharedKeywords = BitSet( IF, FOR, ELSE, THIS, NULL, NEW, SUPER, ABSTRACT, FINAL, PRIVATE, PROTECTED,
    OVERRIDE, EXTENDS, TRUE, FALSE, CLASS, IMPORT, PACKAGE, DO, THROW, TRY, CATCH, FINALLY, WHILE, RETURN )
  final val primTypes = tokenRange(VOID, DOUBLE)
  final val keywords = sharedKeywords | javaOnlyKeywords | primTypes

  /** keywords */
  final val INSTANCEOF = 101;       enter(INSTANCEOF, "instanceof")
  final val CONST = 102;            enter(CONST, "const")

  /** templates */
  final val INTERFACE = 105;        enter(INTERFACE, "interface")
  final val ENUM = 106;             enter(ENUM, "enum")
  final val IMPLEMENTS = 107;       enter(IMPLEMENTS, "implements")

  /** modifiers */
  final val PUBLIC = 110;           enter(PUBLIC, "public")
  final val DEFAULT = 111;          enter(DEFAULT, "default")
  final val STATIC = 112;           enter(STATIC, "static")
  final val TRANSIENT = 113;        enter(TRANSIENT, "transient")
  final val VOLATILE = 114;         enter(VOLATILE, "volatile")
  final val SYNCHRONIZED = 115;     enter(SYNCHRONIZED, "synchronized")
  final val NATIVE = 116;           enter(NATIVE, "native")
  final val STRICTFP = 117;         enter(STRICTFP, "strictfp")
  final val THROWS = 118;           enter(THROWS, "throws")

  /** control structures */
  final val BREAK = 130;            enter(BREAK, "break")
  final val CONTINUE = 131;         enter(CONTINUE, "continue")
  final val GOTO = 132;             enter(GOTO, "goto")
  final val SWITCH = 133;           enter(SWITCH, "switch")
  final val ASSERT = 134;           enter(ASSERT, "assert")

  /** special symbols */
  final val EQEQ = 140
  final val BANGEQ = 141
  final val LT = 142
  final val GT = 143
  final val LTEQ = 144
  final val GTEQ = 145
  final val BANG = 146
  final val QMARK = 147
  final val AMP = 148
  final val BAR = 149
  final val PLUS = 150
  final val MINUS = 151
  final val ASTERISK = 152
  final val SLASH = 153
  final val PERCENT = 154
  final val HAT = 155
  final val LTLT = 156
  final val GTGT = 157
  final val GTGTGT = 158
  final val AMPAMP = 159
  final val BARBAR = 160
  final val PLUSPLUS = 161
  final val MINUSMINUS = 162
  final val TILDE = 163
  final val DOTDOTDOT = 164
  final val AMPEQ = 165
  final val BAREQ = 166
  final val PLUSEQ = 167
  final val MINUSEQ = 168
  final val ASTERISKEQ = 169
  final val SLASHEQ = 170
  final val PERCENTEQ = 171
  final val HATEQ = 172
  final val LTLTEQ = 173
  final val GTGTEQ = 174
  final val GTGTGTEQ = 175

  /** primitive types */
  final val VOID = 180;             enter(VOID, "void")
  final val BOOLEAN = 181;          enter(BOOLEAN, "boolean")
  final val BYTE = 182;             enter(BYTE, "byte")
  final val SHORT = 183;            enter(SHORT, "short")
  final val CHAR = 184;             enter(CHAR, "char")
  final val INT = 185;              enter(INT, "int")
  final val LONG = 186;             enter(LONG, "long")
  final val FLOAT = 187;            enter(FLOAT, "float")
  final val DOUBLE = 188;           enter(DOUBLE, "double")
}
