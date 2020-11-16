case class Project(name: String)
extension (name: String) def dependencies: String = ???
extension (project: Project) def dependencies: String = project.name.dependencies
