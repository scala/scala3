package dotty.dokka

class GenericSignatures extends SingleFileTest("genericSignatures", Seq("class"))

class ObjectSignatures extends SingleFileTest("objectSignatures", Seq("object"))

class TraitSignatures extends SingleFileTest("traitSignatures", Seq("trait"))


// We do not support companion objects properly in tests
class ClassSignatureTestSourceTest extends SingleFileTest("classSignatureTestSource",
  SingleFileTest.all.filterNot(Seq("val", "var", "object").contains))

// TODO we still cannot filter out all constructor-based fields
class SignatureTestSourceTest extends SingleFileTest("signatureTestSource", SingleFileTest.all)

class ModifiersSignatureTest extends SingleFileTest("modifiersSignatureTestSource", SingleFileTest.all)

class Visibility extends SingleFileTest("visibility", SingleFileTest.all)


class GenericMethodsTest extends SingleFileTest("genericMethods", Seq("def"))

class MethodsAndConstructors extends SingleFileTest("methodsAndConstructors", Seq("def"))

class TypesSignatures extends SingleFileTest("typesSignatures", SingleFileTest.all)

class FieldsSignatures extends SingleFileTest("fieldsSignatures", SingleFileTest.all.filter(_ != "object"))

class NestedSignatures extends SingleFileTest("nested", SingleFileTest.all)

class CompanionObjectSignatures extends SingleFileTest("companionObjectSignatures", SingleFileTest.all)

class PackageSymbolSignatures extends SingleFileTest("packageSymbolSignatures", SingleFileTest.all)

class PackageObjectSymbolSignatures extends SingleFileTest("packageObjectSymbolSignatures", SingleFileTest.all.filter(_ != "object"))

class MergedPackageSignatures extends MultipleFileTest(List("mergedPackage1", "mergedPackage2", "mergedPackage3"), List("mergedPackage"), SingleFileTest.all.filter(_ != "object"))

class ExtensionMethodSignature extends SingleFileTest("extensionMethodSignatures", SingleFileTest.all)

class ClassModifiers extends SingleFileTest("classModifiers", SingleFileTest.classlikeKinds)

// class EnumSignatures extends SingleFileTest("enumSignatures", SingleFileTest.all)

class StructuralTypes extends SingleFileTest("structuralTypes", SingleFileTest.members)

class OpaqueTypes extends SingleFileTest("opaqueTypes", SingleFileTest.all)

// class GivenSignatures extends SingleFileTest("givenSignatures", SingleFileTest.all)

class Annotations extends SingleFileTest("annotations", SingleFileTest.all)
