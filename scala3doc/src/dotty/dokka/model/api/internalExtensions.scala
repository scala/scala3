package dotty.dokka
package model
package api

extension (member: Member)
  def withMembers(newMembers: Seq[Member]): Member = member.copy(members = newMembers)

  def updateRecusivly(op: Member => Member): Member =
      val newMembers = member.members.map(_.updateRecusivly(op))
      op(member).withMembers(newMembers)


  def withOrigin(origin: Origin): Member = member.copy(origin = origin)


  def withKind(kind: Kind): Member = member.copy(kind = kind)


  def withNewMembers(newMembers: Seq[Member]): Member =
    member.copy(members = member.members ++ newMembers)

  def withKnownChildren(knownChildren: Seq[LinkToType]): Member =
    member.copy(knownChildren = knownChildren)

  def withNewGraphEdges(edges: Seq[(LinkToType, LinkToType)]): Member =
    member.copy(graph = member.graph ++ edges)

extension (m: Module)
  def updatePackages(op: Seq[Member] => Seq[Member]): Module =
    val newRoot = m.rootPackage.withMembers(op(m.rootPackage.members))
    m.copy(rootPackage = newRoot)

  def updateMembers(op: Member => Member): Module =
     updatePackages(_.map(p => p.updateRecusivly(op)))

  def visitMembers(callback: Member => Unit): Unit =
    def visitClasslike(c: Member): Unit =
      callback(c)
      c.members.foreach(visitClasslike(_))

    visitClasslike(m.rootPackage)
