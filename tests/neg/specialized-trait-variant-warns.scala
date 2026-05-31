//> using options -language:experimental.specializedTraits -Werror

// nopos-error: (warning): Type parameter is both Specialized and avariant. This imposes additional typing restrictions.
// nopos-error: (warning): Type parameter is both Specialized and variant. This imposes additional typing restrictions.

inline trait Bin[-T: Specialized]
inline trait List[+T: Specialized]
