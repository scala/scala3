package dotty.dokka

class GenericSignaftures extends SignatureTest("genericSignatures", Seq("class"))

class ObjectSignatures extends SignatureTest("objectSignatures", Seq("object"))

class TraitSignatures extends SignatureTest("traitSignatures", Seq("trait"))


// We do not support companion objects properly in tests
class ClassSignatureTestSourceTest extends SignatureTest("classSignatureTestSource",
  SignatureTest.all diff Seq("val", "var", "object"))

// TODO we still cannot filter out all constructor-based fields
class SignatureTestSourceTest extends SignatureTest("signatureTestSource", SignatureTest.all)

class ModifiersSignatureTest extends SignatureTest("modifiersSignatureTestSource", SignatureTest.all)

class Visibility extends SignatureTest("visibility", SignatureTest.all)


class GenericMethodsTest extends SignatureTest("genericMethods", Seq("def"))

class MethodsAndConstructors extends SignatureTest("methodsAndConstructors", Seq("def"))

class TypesSignatures extends SignatureTest("typesSignatures", SignatureTest.all)

class FieldsSignatures extends SignatureTest("fieldsSignatures", SignatureTest.all.filterNot(_ == "object"))

class NestedSignatures extends SignatureTest("nested", SignatureTest.all)

class CompanionObjectSignatures extends SignatureTest("companionObjectSignatures", SignatureTest.all)

class PackageSymbolSignatures extends SignatureTest("packageSymbolSignatures", SignatureTest.all)

class PackageObjectSymbolSignatures extends SignatureTest("packageObjectSymbolSignatures", SignatureTest.all.filterNot(_ == "object"))

class MergedPackageSignatures extends SignatureTest("mergedPackage", SignatureTest.all.filterNot(_ == "object"),
  sourceFiles = List("mergedPackage1", "mergedPackage2", "mergedPackage3"))

class ExtensionMethodSignature extends SignatureTest("extensionMethodSignatures", SignatureTest.all)

class ClassModifiers extends SignatureTest("classModifiers", SignatureTest.classlikeKinds)

// class EnumSignatures extends SignatureTest("enumSignatures", SignatureTest.all)

class StructuralTypes extends SignatureTest("structuralTypes", SignatureTest.members)

class OpaqueTypes extends SignatureTest("opaqueTypes", SignatureTest.all)

// class GivenSignatures extends SignatureTest("givenSignatures", SignatureTest.all)

class Annotations extends SignatureTest("annotations", SignatureTest.all)

class InheritanceLoop extends SignatureTest("inheritanceLoop", SignatureTest.all)

class InheritedMembers extends SignatureTest("inheritedMembers2", SignatureTest.all.filter(_ != "class"),
  sourceFiles = List("inheritedMembers1", "inheritedMembers2"))

class ComplexNames extends SignatureTest("complexNames", Seq("def"))

class WrongDocumentationLinks extends SignatureTest("links", Seq("def"))