object NewModifiers/*<-_empty_::NewModifiers.*/ {
  inline val foo/*<-_empty_::NewModifiers.foo.*/ = "foo"
  opaque type A/*<-_empty_::NewModifiers.A#*/ = Int/*->scala::Int#*/
}

opaque type OpaqueB/*<-_empty_::NewModifiers$package.OpaqueB#*/ = Int/*->scala::Int#*/

class NewModifiersClass/*<-_empty_::NewModifiersClass#*/ {
  opaque type C/*<-_empty_::NewModifiersClass#C#*/ = Int/*->scala::Int#*/
  class Nested/*<-_empty_::NewModifiersClass#Nested#*/ {
    opaque type NestedOpaque/*<-_empty_::NewModifiersClass#Nested#NestedOpaque#*/ = Int/*->scala::Int#*/
  }
}

trait NewModifiersTrait/*<-_empty_::NewModifiersTrait#*/ {
  opaque type D/*<-_empty_::NewModifiersTrait#D#*/ = Int/*->scala::Int#*/
}
