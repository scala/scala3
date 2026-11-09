package tests.warn.i22298_separ;

public sealed abstract class AbstractSealed_1 permits A, B, C {}

final class A extends AbstractSealed_1 {}

final class B extends AbstractSealed_1 {}

final class C extends AbstractSealed_1 {}
