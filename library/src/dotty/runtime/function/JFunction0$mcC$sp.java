
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction0$mcC$sp extends JFunction0 {
    abstract char apply$mcC$sp();

    default Object apply() { return (Character) apply$mcC$sp(); }
}
