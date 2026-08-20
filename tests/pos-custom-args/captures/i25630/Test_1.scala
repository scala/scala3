package test
import caps.*

class A extends ExclusiveCapability

class C(val f: (s: String) => A^{fresh})
