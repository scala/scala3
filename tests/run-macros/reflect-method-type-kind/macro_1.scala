trait Foo
trait Bar

object Methods:
  def implicitMethod(implicit foo: Foo, int: Int): Bar = ???
  def contextualMethod(using foo: Foo, int: Int): Bar = ???
  def plainMethod(foo: Foo, int: Int): Bar = ???

object Macro:
  import scala.quoted._
  inline def macroCall(): Unit = ${ macroCallImpl }
  def macroCallImpl(using Quotes): Expr[Unit] =
    testReadingMethodTypeKind
    testCreatingMethodTypeKind
    '{()}

  def testReadingMethodTypeKind(using Quotes) =
    import quotes.reflect._
    def getFromMethods(name: String): TypeRepr =
      val typeRepr =  TypeRepr.of[Methods.type]
      val symbol = 
        typeRepr.typeSymbol.methodMember(name).headOption.getOrElse(
          typeRepr.typeSymbol.fieldMember(name)
        )
      typeRepr.memberType(symbol)
    
    assert(getFromMethods("implicitMethod").asInstanceOf[MethodType].isImplicit)
    assert(!getFromMethods("implicitMethod").asInstanceOf[MethodType].isContextual)
    assert(getFromMethods("implicitMethod").asInstanceOf[MethodType].methodTypeKind == MethodTypeKind.Implicit)

    assert(getFromMethods("contextualMethod").asInstanceOf[MethodType].isImplicit)
    assert(getFromMethods("contextualMethod").asInstanceOf[MethodType].isContextual)
    assert(getFromMethods("contextualMethod").asInstanceOf[MethodType].methodTypeKind == MethodTypeKind.Contextual)

    assert(!getFromMethods("plainMethod").asInstanceOf[MethodType].isImplicit)
    assert(!getFromMethods("plainMethod").asInstanceOf[MethodType].isContextual)
    assert(getFromMethods("plainMethod").asInstanceOf[MethodType].methodTypeKind == MethodTypeKind.Plain)


  def testCreatingMethodTypeKind(using Quotes) =
    import quotes.reflect._
    val paramTypes = List(TypeRepr.of[Foo], TypeRepr.of[Int])
    val resType = TypeRepr.of[Bar]
    val implicitMethodType = MethodType.apply(MethodTypeKind.Implicit)(List("foo", "int"))(mt => paramTypes, mt => resType)
    assert(implicitMethodType.isImplicit)
    assert(!implicitMethodType.isContextual)
    assert(implicitMethodType.methodTypeKind == MethodTypeKind.Implicit)
    assert(implicitMethodType.methodTypeKind != MethodTypeKind.Contextual)
    assert(implicitMethodType.methodTypeKind != MethodTypeKind.Plain)


    val contextualMethodType = MethodType.apply(MethodTypeKind.Contextual)(List("foo", "int"))(mt => paramTypes, mt => resType)
    assert(contextualMethodType.isImplicit)
    assert(contextualMethodType.isContextual)
    assert(contextualMethodType.methodTypeKind != MethodTypeKind.Implicit)
    assert(contextualMethodType.methodTypeKind == MethodTypeKind.Contextual)
    assert(contextualMethodType.methodTypeKind != MethodTypeKind.Plain)

    val plainMethodType = MethodType.apply(MethodTypeKind.Plain)(List("foo", "int"))(mt => paramTypes, mt => resType)
    assert(!plainMethodType.isContextual)
    assert(!plainMethodType.isImplicit)
    assert(plainMethodType.methodTypeKind != MethodTypeKind.Implicit)
    assert(plainMethodType.methodTypeKind != MethodTypeKind.Contextual)
    assert(plainMethodType.methodTypeKind == MethodTypeKind.Plain)
