// {
//   "projects": [
//     {
//       "name": "app",
//       "dependsOn": [
//         "macros"
//       ],
//       "scalaVersion": "2.13.12"
//     },
//     {
//       "name": "macros",
//       "scalaVersion": "2.13.12"
//     }
//   ]
// }

lazy val app = project.in(file("app"))
  .dependsOn(macros)

lazy val macros = project.in(file("macros"))
