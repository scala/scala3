

def f1[T][U](x: T, y: U): (T, U) = (x, y)
def f2[T](x: T)[U](y: U): (T, U) = (x, y)


def foo[T, U][V](x: T): U = ???

@main def test = foo[Int] // should probably not work (and doesn't)

// check ce qui se passe quand appelée
// def f3[T][U]: [X] => F[X]
// done: voir higherKindedReturn.scala

// reflechir quoi changer dans la spe pour rendre le currying/interweaving dans la doc
// https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html
//
// Only need to change definitions, as applications can also be of that form, see higherKindedReturn.scala:11
/* 
 * in https://www.scala-lang.org/files/archive/spec/2.13/04-basic-declarations-and-definitions.html#function-declarations-and-definitions
 * Change:
 * 
 * FunSig                 ::=  id ParamClauses
 * ParamClauses           ::=  ParamClause {ParamClause}
 * ParamClause            ::=  TermParamClause | TypeParamClause | UsingParamClause
 * TermParamClause        ::=  [nl] ‘(’ [TermParams] ‘)’                              //note: allows ()
 * TypeParamClause        ::=  [nl] ‘[’ TypeParams ‘]’
 * UsingParamClause       ::=  [nl] ‘(’ ‘using’ TermParams ‘)’
 * 
 * (slightly simpler but should be equivalent to changes made in Parser.scla)
 */
// same for classes, je suis un peu confus par la syntaxe ci-dessous, notament Annotation et AccessModifiers, ils sont pas là en scala 3, non ?
// https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#class-definitions

// impl param de classe, déjà Parser


// Que faire de  TypeDcl ::=  id [TypeParamClause] {FunParamClause} TypeBounds [‘=’ Type]  //TODO: change to {ParamClauses} ?


/* Endroits à changer: (toujour creation et application)
    function definition     Done
    function application    Already works
    class definition        In Progress
    class instantiation     Might already work
    given definition        Todo
    given application(?)    Doesn't exist ? or add multiple params to usings ?
    type declarations       To be determined
    type instantiation      Doesn't exist ?
    extension definition    Todo   
    extension application   Todo
 */



// étapes pour "convaincre"
// 1) exemples simples, et exemples d'utilité
// 2) expliquer intuitivement
// 3) update la doc
// 4) PR et tout le tralala


//potentiellement cette feature en experimental

// eta expension:
val f = foo //should work and doesn't preserve type params yet