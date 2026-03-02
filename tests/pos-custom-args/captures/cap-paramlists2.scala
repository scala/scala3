import language.experimental.captureChecking

trait Bar:
  type C^

def useFoo[D^](x: Bar { type C = {D} } ): Any^{x.C} = ???