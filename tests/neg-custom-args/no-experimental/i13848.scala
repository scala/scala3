import annotation.experimental

@main
def run(): Unit = f // error

@experimental
def f = 2
