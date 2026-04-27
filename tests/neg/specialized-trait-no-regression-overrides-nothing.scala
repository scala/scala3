trait A1:
    def foo = "A"

trait B1:
    override def foo = "B" // error: foo overrides nothing

inline trait A2:
    def foo = "A"

inline trait B2:
    override def foo = "B" // error: foo overrides nothing
