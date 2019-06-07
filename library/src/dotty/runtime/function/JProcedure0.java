
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

import scala.runtime.BoxedUnit;

@FunctionalInterface
public interface JProcedure0 extends JFunction0<BoxedUnit>, java.io.Serializable {
    void applyVoid();

    default BoxedUnit apply() {
        applyVoid();
        return BoxedUnit.UNIT;
    }
}
