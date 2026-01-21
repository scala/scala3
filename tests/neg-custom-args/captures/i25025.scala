import language.experimental.captureChecking

def foo[c^](c: AnyRef^): Int = 0 // error
