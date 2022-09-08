package scala.annotation

import scala.annotation.Annotation

/**
 * This annotation is applicable on type parameter declarations and instructs the compiler to
 * make best-effort in keeping the annotated parameter as precise as possible.
 * This feature is currently experimental. See SIP for documentation.
 * TODO: complete documentation reference here after SIP is approved.
 */
@experimental
class precise extends Annotation {

}
