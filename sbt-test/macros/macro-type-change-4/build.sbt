// {
//   "projects": [
//     {
//       "name": "app",
//       "dependsOn": [
//         "macros",
//         "A"
//       ],
//       "scalaVersion": "2.13.12"
//     },
//     {
//       "name": "macros",
//       "scalaVersion": "2.13.12"
//     },
//     {
//       "name": "A",
//       "scalaVersion": "2.13.12"
//     }
//   ]
// }

lazy val app = project.in(file("app"))
  .dependsOn(macros, A)

lazy val macros = project.in(file("macros"))

lazy val A = project.in(file("A"))
