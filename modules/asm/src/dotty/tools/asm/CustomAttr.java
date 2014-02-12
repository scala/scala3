/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 */

package dotty.tools.asm;

import dotty.tools.asm.Attribute;

/**
 * A subclass of ASM's Attribute for the sole purpose of accessing a protected field there.
 *
 */
public class CustomAttr extends Attribute {

    public CustomAttr(final String type, final byte[] value) {
        super(type);
        super.value = value;
    }

}
