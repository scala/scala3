

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
// 
// change text to explain application notably when only passed one clause of type params when multiple type clauses expected

// impl param de classe, déjà Parser: Done


//add function call to chainedParams

/* Endroits à changer: (toujour creation et application ?)
    function definition     Done
    function application    Already works
    class definition        Standby
        primary constr          Standby
        auxiliary constr        Standby
    class instantiation     Standby (Might already work // see new)
    given definition        Todo //yes if given alias, Standby otherwise given name[](using ...)[](using ...) : Classe[Params]
    given application(?)    Already works ?/standby Doesn't exist ? or add multiple params to usings ? (using name: Class[T]{val x}[U])
    type declarations       Standby: because no transformation from F[X,Y] <-> F[X][Y] //is [X,Y] =>> F[X][Y] valid ?
    type instantiation      Doesn't exist ?
    extension definition    Todo // probably only on right side, since classes do not keep info about params
    extension application   Todo: tester
 */



// étapes pour "convaincre"
// 1) exemples simples, et exemples d'utilité   TODO
// 2) expliquer intuitivement                   TODO
// 3) update la doc                             Done
// 4) PR et tout le tralala                     TODO


//potentiellement cette feature en experimental

// eta expension:
val f = foo //should work and doesn't preserve type params yet





//new:
// mettre de coté la partie sur les classes: Done
// fork le repo de la doc est faire changements appropriés: Done: https://github.com/scala/scala/pull/9792
// check overloading resolution
// Applications.scala resolveMapped //What to do here?
//                      methType
// ProtoTypes.scala

// check ne passer aucun arguments à une fonction // Je me rappelle pas ce que c'était ...
// voir fichier overload dans pos/interweaving_   // Done
// voir methType                                  // Done ?
// check occurances de PolyType et PolyProto pour être sûr que ça fonctionne bien avec les [T][U]
//    PolyType:  Done in Application.scala
//    PolyProto: Done in Application.scala
// testCompilation
//    Pas eu le temps, mais la PR a lancer des tests, donc 5 qui ont ratés dans une phase

// check def foo[T][T]
//
// is def foo[T](using T)(x: Int) valid ?
/*
 * check: done see newline.scala
 * def foo
 *  [T]
 *  (x: T)
 *  [U]
 * 
 * and similar
 * 
*/


// should we allow fun[L <: List[T]][T] as alias for something like fun[F[_] <: List[]][T][L <: F[T]]
// I'm not sure it's useful



// dans docu check "method type" et "poly(morphic) type/method" pour voir si suppositions fausses
// 

// tryApply:
//   si f pas fonction, essaie f.apply(...)



// next:
// Relire toute docu: Done
// voir eta expension dans la doc, pour voir comment changer pour polymorphique, ajouter texte qui explique que passe parfois param de type si besoin
// mettre un peu au propre texte PR -> trouver exemples
// essayer given def: Paused


/**
 * A few comments:
    Should handle and test for extension definitions as well.
    Should test of overriding and signature check and proper report when signature does not match.
    I suggest opening a thread on https://contributors.scala-lang.org/ to discuss this language change. 
 * 
 * 
 * 
 * 
 */


// type Z = X =>> F[X]
// def name: [X] => (X, X) => (U)


// def foo[T](x: T): T
// val bar = foo


// check overload: (should fail)
// def foo(x: Int)(y: String)
// def foo(x: Int)[T](y: String)
// def foo[T](x: Int)(y: String)



// next
// adaptations faitent dans Typer.scala adapt (regarder la fin ou il y a le "vrai" body de la fonction)
// pour rajouter le cas valId3 qui typecheck (les autres cas ce sera pour après)
// https://github.com/lampepfl/dotty/pull/4672
