
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcFI$sp extends JFunction1<Object, Object> {
    abstract float apply$mcFI$sp(int v1);

    default Object apply(Object t) { return (Float) apply$mcFI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)); }
}
