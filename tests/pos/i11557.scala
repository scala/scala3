type MyEncoder

class MyContext:
  given intEncoder: MyEncoder = ???

def doEncoding(ctx: MyContext): Unit =
  import ctx.{*, given}
  summon[MyEncoder]
  summonInlineMyEncoder()

inline def summonInlineMyEncoder(): Unit =
  compiletime.summonInline[MyEncoder]
