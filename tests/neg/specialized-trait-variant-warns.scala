//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits -Werror

// nopos-error: no warnings under Werror

inline trait Bin[-T: Specialized] // warn
inline trait List[+T: Specialized] // warn
