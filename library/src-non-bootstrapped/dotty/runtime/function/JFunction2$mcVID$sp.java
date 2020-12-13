
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcVID$sp extends JFunction2<Object, Object, Object> {
    abstract void apply$mcVID$sp(int v1, double v2);

    default Object apply(Object v1, Object v2) { apply$mcVID$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2)); return scala.runtime.BoxedUnit.UNIT; }
}
