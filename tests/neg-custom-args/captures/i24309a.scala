import language.experimental.captureChecking
def runOps[C^](ops: List[() ->{C} Unit]): Unit = ???
def runOps1[C^](xs: Object^{C}): Unit = runOps[{C}](???)  // ok
def runOps2[C^](): Unit = runOps[{C}](???)  // error
