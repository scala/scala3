//> using options -Xfatal-warnings -Wunused:all

def aMethod() =
  doStuff { (x) => x } // error
