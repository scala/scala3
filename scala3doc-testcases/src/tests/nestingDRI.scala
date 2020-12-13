package tests.nestingDRI

trait TestClass

class A:
  class B
  object B:
    object C
    class C:
      object D


class AA:
  object B:
    class C:
      object D