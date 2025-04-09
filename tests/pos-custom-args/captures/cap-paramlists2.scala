import language.experimental.captureChecking

trait Bar:
  cap type C

def useFoo[cap D](x: Bar { cap type C = {D} } ): Any^{x.C} = ???