
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction2$mcJJD$sp extends JFunction2 {
    abstract long apply$mcJJD$sp(long v1, double v2);

    default Object apply(Object v1, Object v2) { return (Long) apply$mcJJD$sp((Long) v1, (Double) v2); }
}
