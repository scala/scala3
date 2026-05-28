//> using options -language:experimental.specializedTraits -Werror

// nopos-error: (warning): Type parameter is both Specialized and contravariant. This imposes additional typing restrictions.

inline trait Bin[-T: Specialized]
