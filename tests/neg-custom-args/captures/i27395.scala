//> using options -Ycc-debug -Ycc-verbose

// Printing the error used to loop
class A
def id[X^](x: A^{X}): A^{X} = x
def test(y: A^): A^{} = id(y) // error
