trait Template[+T <: Template[T]]:
   type Clone <: T { type Clone = Template.this.Clone }
   val self :Clone

type Food = Template[_]

class Ham extends Template[Ham]:
   type Clone = Ham
   val self = this

def eat[F <: Template[F]](food :F) :F = food.self.self

val ham = new Ham
val food :Food = ham

def test =  // error
  eat(ham)
  eat(food.self)
