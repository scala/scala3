package tests

package implicitConversions2

class Methods //unexpected
{
  def shouldBeImplicitlyAdded1: String
     = ???
  val shouldBeImplicitlyAdded2: String
     = ???
  class ShouldBeImplicitlyAdded3
  type ShouldBeImplicitlyAdded4
}

class OuterClass //unexpected
{
  implicit def conversionMethodWithOneParam(param: ClassWithConversionWithOneParam): Methods //unexpected
    = ???

  class ClassWithConversionWithOneParam //unexpected

  class ClassWithConversionWithProperType extends InheritedClass //unexpected

  class InheritedClass //unexpected

  object InheritedClass //unexpected
  {
    implicit def conversionMethodWithProperType: Conversion[ClassWithConversionWithProperType, Methods] //unexpected
      = ???
  }

  given conversionFromVal: Conversion[ClassWithConversionFromVal, Methods] with //unexpected
  {
    def apply(a: ClassWithConversionFromVal): Methods //unexpected
    = ???
  }

  class ClassWithConversionFromVal //unexpected

}