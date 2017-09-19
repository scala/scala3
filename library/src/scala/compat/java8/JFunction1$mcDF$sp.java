
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction1$mcDF$sp extends JFunction1 {
    abstract double apply$mcDF$sp(float v1);

    default Object apply(Object t) { return (Double) apply$mcDF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)); }
}
