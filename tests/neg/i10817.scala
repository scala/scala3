import annotation.static

class T with
  @static val foo = 10 // error

val x = (new T).foo