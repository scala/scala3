case class Project(name: String)
extension (name: String) def dependencies = ???
extension (project: Project) def dependencies = project.name.dependencies // error, following a cyclic reference warning