
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcVDJ$sp extends JFunction2<Object, Object, Object> {
    abstract void apply$mcVDJ$sp(double v1, long v2);

    default Object apply(Object v1, Object v2) { apply$mcVDJ$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)); return scala.runtime.BoxedUnit.UNIT; }
}
