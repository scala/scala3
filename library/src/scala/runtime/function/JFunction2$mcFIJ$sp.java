
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.function;

@FunctionalInterface
public interface JFunction2$mcFIJ$sp extends JFunction2<Object, Object, Object> {
    abstract float apply$mcFIJ$sp(int v1, long v2);

    default Object apply(Object v1, Object v2) { return (Float) apply$mcFIJ$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)); }
}
