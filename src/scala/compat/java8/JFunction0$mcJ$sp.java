
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction0$mcJ$sp extends JFunction0 {
    abstract long apply$mcJ$sp();

    default Object apply() { return (Long) apply$mcJ$sp(); }
}
