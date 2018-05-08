// Test that function types ending in SIP-23 singleton types are understood correctly.
class E extends (Int => 1) // error
