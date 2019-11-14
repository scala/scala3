package selfs

class B/*<-selfs::B#*/

class C1/*<-selfs::C1#*/ extends B/*->selfs::B#*/ { self/*<-local0*/ =>
}

class C2/*<-selfs::C2#*/ extends B/*->selfs::B#*/ { self/*<-local1*/: B/*->selfs::B#*/ =>
}

class C3/*<-selfs::C3#*/ extends B/*->selfs::B#*/ { self/*<-local2*/: B/*->selfs::B#*/ with C1/*->selfs::C1#*/ =>
}

class C6/*<-selfs::C6#*/ extends B/*->selfs::B#*/ { this: B/*->selfs::B#*/ =>
}
