trait NodeK { type FBound }
trait Fixed[+N <: NodeK]

type Bound1 <: FP1
type FP1     = Fixed[Node1]
type Node1   = NodeK { type FBound <: Bound1 } // was-error

type FP2     = Fixed[Node2]                    // was-error
type Bound2 <: FP2
type Node2   = NodeK { type FBound <: Bound2 }

type Node3   = NodeK { type FBound <: Bound3 }
type FP3     = Fixed[Node3]
type Bound3 <: FP3

type Bound4 <: FP4
type Node4   = NodeK { type FBound <: Bound4 } // was-error
type FP4     = Fixed[Node4]

type FP5     = Fixed[Node5]                    // was-error
type Node5   = NodeK { type FBound <: Bound5 }
type Bound5 <: FP5

type Node6   = NodeK { type FBound <: Bound6 }
type Bound6 <: FP6
type FP6     = Fixed[Node6]
