def f1: Int = { def g = 0 } // error: Found: Unit   Required: Int
def f2: Int = { extension (x: Int) def g = 0 } // error: Found: Unit   Required: Int
