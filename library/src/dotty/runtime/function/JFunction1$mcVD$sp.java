
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcVD$sp extends JFunction1<Object, Object> {
    abstract void apply$mcVD$sp(double v1);

    default Object apply(Object t) { apply$mcVD$sp(scala.runtime.BoxesRunTime.unboxToDouble(t)); return scala.runtime.BoxedUnit.UNIT; }
}
