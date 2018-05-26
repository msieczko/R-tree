package jps

case class Node[T](boundingRect: Rectangle)

object Node {
    def newRoot[T]: Leaf[T] = Leaf(Vector.empty, Rectangle.maxRect)
}

case class Leaf[T](children: Vector[Entry[T]], override val boundingRect: Rectangle)
    extends Node[T](boundingRect)

case class Branch[T](children: Vector[Node[T]], override val boundingRect: Rectangle)
    extends Node[T](boundingRect)


//case class Node[T](parent: Node[T])
//case class Leaf[T](override val parent: Node[T], children: Vector[Entry[T]], boundingRect: Rectangle)
//    extends Node[T](parent)
//
//case class Branch[T](override val parent: Node[T], children: Vector[Node[T]], boundingRect: Rectangle)
//    extends Node[T](parent)
