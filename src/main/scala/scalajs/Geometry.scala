package scalajs

object Geometry {
    case class Point(x : Js[Double], y : Js[Double]) extends JsObject[Point]
    case class Rectangle(topLeft : Js[Point], bottomRight : Js[Point]) extends JsObject[Rectangle]
    case class Circle(center : Js[Point], radius : Js[Double]) extends JsObject[Circle]
    case class Line(from : Js[Point], to : Js[Point]) extends JsObject[Line]

    implicit def toPoint(p : Js[Point]) = Point(GetField(p, "x"), GetField(p, "y"))
    implicit def toRectangle(r : Js[Rectangle]) = Rectangle(GetField(r, "topLeft"), GetField(r, "bottomRight"))
    implicit def toCircle(c : Js[Circle]) = Circle(GetField(c, "center"), GetField(c, "radius"))
    implicit def toLine(l : Js[Line]) = Rectangle(GetField(l, "from"), GetField(l, "to"))
}
