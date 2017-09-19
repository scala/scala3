
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction1$mcFI$sp extends JFunction1 {
    abstract float apply$mcFI$sp(int v1);

    default Object apply(Object t) { return (Float) apply$mcFI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)); }
}
