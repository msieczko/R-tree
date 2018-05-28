package jps

trait Bound {
    def coords: Coordinates
    def dims: Dimensions
    def x: Double = coords.x
    def y: Double = coords.y
    def x2: Double = x + width
    def y2: Double = y + height
    def width: Double = dims.dx
    def height: Double = dims.dy
}

case class Coordinates(x: Double, y: Double)

case class Dimensions(dx: Double, dy: Double)

/**
  * @param coords Left bottom corner of the rectangle
  * @param dims   Dimensions of the rectangle
  */
case class Rectangle(coords: Coordinates, dims: Dimensions) extends Bound {
    def enlargementToFit(rect: Rectangle): Double = {
        val deltaX = if (x2 < rect.x2) {
            rect.x2 - x2
        } else if (x2 > rect.x2) {
            x - rect.x
        } else 0
        val deltaY = if (y2 < rect.y2) {
            rect.y2 - y2
        } else if (y2 > rect.y2) {
            y - rect.y
        } else 0

        val largeArea = (deltaX + width) * (deltaY + height)
        largeArea - area
    }

    def area(): Double = width * height
}

object Rectangle {
    val maxRect: Rectangle = {
        val minC = -Math.sqrt(Double.MaxValue)
        val d = -2.0 * minC
        Rectangle(Coordinates(minC, minC), Dimensions(d, d))
    }

    val minRect: Rectangle = Rectangle(Coordinates(0.0, 0.0), Dimensions(0.0, 0.0))
}

/**
  * Point is a rectangle with dimensions set to 0.0
  *
  * @param coords Point coordinates
  */
case class Point(override val coords: Coordinates) extends Bound {
    override def dims: Dimensions = Dimensions(0.0, 0.0)
    override def x2: Double = x
    override def y2: Double = y
    override def width: Double = 0.0
    override def height: Double = 0.0
}
