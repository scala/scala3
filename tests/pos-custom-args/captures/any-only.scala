import caps.{any, SharedCapability}

val x: Object^{any.only[SharedCapability]} = Object()
