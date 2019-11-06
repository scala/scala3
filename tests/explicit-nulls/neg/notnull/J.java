import java.util.*;
import notnull.NotNull;

public class J {

    @NotNull
    // TODO: remove annotaion after #7483
    // JavaParser will never assign ConstantType to fields currently.
    public static final String k = "k";

    @NotNull
    public static String l = "l";
}
