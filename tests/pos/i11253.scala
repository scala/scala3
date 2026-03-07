// --- Valid cases related to #11253/#9541 ---

trait U[E <: Enum[E]] extends Enum[E]
trait TValid extends Enum[E9]

enum E4 extends Enum[E4] { case X }

enum E5[+A] extends Enum[E5[?]] { case X extends E5[Nothing] }

enum E6[A] extends Enum[E6[?]] { case X extends E6[Unit] }

enum E9 extends Enum[E9] with TValid { case X }

enum E10 extends U[E10] { case X }
