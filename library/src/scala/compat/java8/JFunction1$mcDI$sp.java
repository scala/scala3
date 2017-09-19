
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction1$mcDI$sp extends JFunction1 {
    abstract double apply$mcDI$sp(int v1);

    default Object apply(Object t) { return (Double) apply$mcDI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)); }
}
