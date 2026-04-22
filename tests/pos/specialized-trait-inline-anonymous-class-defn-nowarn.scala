//> using options -language:experimental.specializedTraits -Werror
inline trait T1[T: Specialized]

inline def foo[F: Specialized] = new T1[F]() {}       // By default this warns Anonymous class will be defined at each use site, which may lead to a larger number of classfiles. This is not true for Specialized traits as we share the $impl$ class instances.
inline def foo[F: Specialized, E] = new T1[F, E]() {} // This should also not warn even though E is not specialized 
