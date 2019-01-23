/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction1$mcVF$sp extends JFunction1 {
    abstract void apply$mcVF$sp(float v1);

    default Object apply(Object t) { apply$mcVF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)); return scala.runtime.BoxedUnit.UNIT; }
}
