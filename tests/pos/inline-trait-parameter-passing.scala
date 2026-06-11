inline trait A:
	def foo() = 10

class B extends A:
	def bar() = 1000
	
def some_function(x: A) =
	x.foo()

@main def main = 
	val b = B()
	some_function(b)
