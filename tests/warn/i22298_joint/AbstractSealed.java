package tests.warn.i22298_joint;

public sealed abstract class AbstractSealed permits A, B, C {}

final class A extends AbstractSealed {}

final class B extends AbstractSealed {}

final class C extends AbstractSealed {}