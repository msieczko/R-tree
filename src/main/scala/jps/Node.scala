package jps

case class Node[T](boundingRect: Rectangle)

object Node {
    def newRoot[T]: Leaf[T] = Leaf(Vector.empty, Rectangle.maxRect)
}

case class Leaf[T](children: Vector[Entry[T]], override val boundingRect: Rectangle)
    extends Node[T](boundingRect)

object Leaf {
    def apply[T](singleChild: Entry[T]): Leaf[T] = {
        apply(Vector[Entry[T]](singleChild))
    }


    def apply[T](children: Vector[Entry[T]]): Leaf[T] = {
        if (children.nonEmpty) {
            Leaf(children, getBoundingRect(children))
        } else {
            Leaf(children, Rectangle.maxRect)
        }
    }

    private def getBoundingRect[T](children: Vector[Entry[T]]): Rectangle = {
        def create[T](child: Entry[T], remainingChildren: Vector[Entry[T]], boundingRect: Rectangle): Rectangle = {
            val newX = Math.min(Math.min(child.rect.x, child.rect.x2), Math.min(boundingRect.x, boundingRect.x2))
            val newY = Math.min(Math.min(child.rect.y, child.rect.y2), Math.min(boundingRect.y, boundingRect.y2))

            if (remainingChildren.nonEmpty) {
                create(children.head, children.tail, boundingRect)
            } else {
                boundingRect
            }
        }

        create(children.head, children.tail, children.head.rect)
    }


}

case class Branch[T](children: Vector[Node[T]], override val boundingRect: Rectangle)
    extends Node[T](boundingRect)


//case class Node[T](parent: Node[T])
//case class Leaf[T](override val parent: Node[T], children: Vector[Entry[T]], boundingRect: Rectangle)
//    extends Node[T](parent)
//
//case class Branch[T](override val parent: Node[T], children: Vector[Node[T]], boundingRect: Rectangle)
//    extends Node[T](parent)
