
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcIII$sp extends JFunction2<Object, Object, Object> {
    abstract int apply$mcIII$sp(int v1, int v2);

    default Object apply(Object v1, Object v2) { return (Integer) apply$mcIII$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)); }
}
