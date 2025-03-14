import language.experimental.captureChecking

trait Bar:
  cap C

def useFoo[cap D](x: Bar { cap C = D} ): Any^{x.C} = ???