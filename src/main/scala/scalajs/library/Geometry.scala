package scalajs.library

import scalajs._

object Geometry {
    case class Point(x : Js[Double], y : Js[Double]) extends JsObject
    case class Rectangle(topLeft : Js[Point], bottomRight : Js[Point]) extends JsObject
    case class Circle(center : Js[Point], radius : Js[Double]) extends JsObject
    case class Line(from : Js[Point], to : Js[Point]) extends JsObject

    implicit def toPoint(p : Js[Point]) = Point(p ! "x", p ! "y")
    implicit def toRectangle(r : Js[Rectangle]) = Rectangle(r ! "topLeft", r ! "bottomRight")
    implicit def toCircle(c : Js[Circle]) = Circle(c ! "center", c ! "radius")
    implicit def toLine(l : Js[Line]) = Rectangle(l ! "from", l ! "to")
}
