package scalajs.library

import scalajs._
import scalajs.Terms._ // TODO: Get rid of this and make Terms private

object Pattern {

    sealed abstract class Maybe[+T](val tag : Js[String]) extends JsObject
    case object Empty extends Maybe[Nothing]("Empty")
    case class Full[T](value : Js[T]) extends Maybe[T]("Full")

    implicit def toMaybe[T](maybe : Js[_ <: Maybe[T]]) = new {
        def switch[R](f : PartialFunction[Maybe[T], Js[R]]) : Js[R] = Let(maybe, { m : Js[_ <: Maybe[T]] =>
            val g : PartialFunction[Maybe[T], Js[R]] = (x : Maybe[T]) => x match {
                case _ => Throw[String, R]("No match")
            }
            val h : (Maybe[T]) => Js[R] = f orElse g
            If(Binary(Equal, GetField(m, "tag"), "Empty"),
                h(Empty),
                h(Full(GetField(m, "value")))
            )
        })
    }
}
