
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcVIJ$sp extends JFunction2<Object, Object, Object> {
    abstract void apply$mcVIJ$sp(int v1, long v2);

    default Object apply(Object v1, Object v2) { apply$mcVIJ$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)); return scala.runtime.BoxedUnit.UNIT; }
}
