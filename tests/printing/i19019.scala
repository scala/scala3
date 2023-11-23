object ObjectWithSelf:
  object StaticObjectNoSelf:
    def foo: Any = this
  end StaticObjectNoSelf

  object StaticObjectWithSelf:
    self =>

    def foo: Any = self
  end StaticObjectWithSelf

  class Container:
    object NonStaticObjectNoSelf:
      def foo: Any = this
    end NonStaticObjectNoSelf

    object NonStaticObjectWithSelf:
      self =>

      def foo: Any = self
    end NonStaticObjectWithSelf
  end Container
end ObjectWithSelf
