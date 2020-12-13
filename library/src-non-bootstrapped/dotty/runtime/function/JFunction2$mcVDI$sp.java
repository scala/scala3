
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcVDI$sp extends JFunction2<Object, Object, Object> {
    abstract void apply$mcVDI$sp(double v1, int v2);

    default Object apply(Object v1, Object v2) { apply$mcVDI$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)); return scala.runtime.BoxedUnit.UNIT; }
}
