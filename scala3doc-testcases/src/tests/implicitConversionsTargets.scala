package tests

package implicitConversions2

import implicitConversionsSources.Methods

class OuterClass
{
  implicit def conversionMethodWithOneParam(param: ClassWithConversionWithOneParam): Methods
    = ???

  class ClassWithConversionWithOneParam

  class ClassWithConversionWithProperType

  object ClassWithConversionWithProperType
  {
    implicit def conversionMethodWithProperType: Conversion[ClassWithConversionWithProperType, Methods]
       = ???
  }

  given conversionFromVal as Conversion[ClassWithConversionFromVal, Methods]
  {
    def apply(a: ClassWithConversionFromVal): Methods
    = ???
  }

  class ClassWithConversionFromVal

}