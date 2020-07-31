package dotty.dokka

class GenericSignatures extends SingleFileTest("genericSignatures", Seq("class"))

class ObjectSignatures extends SingleFileTest("objectSignatures", Seq("object"))

class TraitSignatures extends SingleFileTest("traitSignatures", Seq("trait"))


// We do not support companion objects properly in tests
class ClassSignatureTestSourceTest extends SingleFileTest("classSignatureTestSource", 
  SingleFileTest.all.filterNot(Seq("val", "var", "object").contains))

// TODO we still cannot filter out all constructor-based fields
class SignatureTestSourceTest extends SingleFileTest("signatureTestSource", SingleFileTest.all)

class ModifiersSignatureTest extends SingleFileTest("modifiersSignatureTestSource", SingleFileTest.all.filter(_ != "object"))


class GenericMethodsTest extends SingleFileTest("genericMethods", Seq("def"))

class MethodsAndConstructors extends SingleFileTest("methodsAndConstructors", Seq("def")) 

class TypesSignatures extends SingleFileTest("typesSignatures", SingleFileTest.all)

class FieldsSignatures extends SingleFileTest("fieldsSignatures", SingleFileTest.all.filter(_ != "object"))

class NestedSignatures extends SingleFileTest("nested", SingleFileTest.all)
class CompanionObjectSignatures extends SingleFileTest("companionObjectSignatures", SingleFileTest.all)
