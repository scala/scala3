import language.experimental.erasedDefinitions

def t1 = [T] => (erased t: T) => t // error
def t2 = [T, U] => (t: T, erased u: U) => u // error
