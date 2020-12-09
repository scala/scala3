
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcIJ$sp extends JFunction1<Object, Object> {
    abstract int apply$mcIJ$sp(long v1);

    default Object apply(Object t) { return (Integer) apply$mcIJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)); }
}
