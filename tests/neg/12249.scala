
export java.util.UUID

def bar = println(new UUID(1, 2)) // OK

def foo = println(UUID.randomUUID()) // error
//                ^^^^
//                Not found: UUID

