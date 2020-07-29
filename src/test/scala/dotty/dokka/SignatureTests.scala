package dotty.dokka

class GenericSignatures extends SingleFileTest("genericSignatures", Seq("class"))

class ObjectSignatures extends SingleFileTest("objectSignatures", Seq("object"))

class TraitSignatures extends SingleFileTest("traitSignatures", Seq("trait"))


class ClassSignatureTestSourceTest extends SingleFileTest("classSignatureTestSource", SingleFileTest.all)


class SignatureTestSourceTest extends SingleFileTest("signatureTestSource", SingleFileTest.all)

class ModifiersSignatureTest extends SingleFileTest("modifiersSignatureTestSource", SingleFileTest.all)


class GenericMethodsTest extends SingleFileTest("genericMethods", Seq("def"))

class MethodsAndConstructors extends SingleFileTest("methodsAndConstructors", Seq("def"))