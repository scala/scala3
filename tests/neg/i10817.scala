import annotation.static

class T:
  @static val foo = 10 // error

val x = (new T).foo