package scalajs.library

import scalajs._
import scala.Left
import scalajs.Terms.Throw
import scalajs.Terms.JsRecord
import scalajs.Terms.GetField
import scalajs.Terms.If
import scalajs.Terms.Binary
import scalajs.Terms.Equal
import scalajs.Terms.Let
import scala.Some
import scala.Right

object Pattern {
    /*
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
    */


    implicit def fromOption[T](option : Option[Js[T]]) : Js[Option[T]] = option match {
        case None => JsRecord[Option[T]](new JsObject { val tag = "None" })
        case Some(x) => JsRecord[Option[T]](new JsObject { val tag = "Some"; val value = x })
    }

    implicit def toOption[T](option : Js[_ <: Option[T]]) = new {
        def switch[R](f : PartialFunction[Option[Js[T]], Js[R]]) : Js[R] = Let(option, { m : Js[_ <: Option[T]] =>
            val g : PartialFunction[Option[Js[T]], Js[R]] = (x : Option[Js[T]]) => x match {
                case _ => Throw[String, R]("No match")
            }
            val h : (Option[Js[T]]) => Js[R] = f orElse g
            If(Binary(Equal, GetField(m, "tag"), "None"),
                h(None),
                h(Some(GetField(m, "value")))
            )
        })
    }

    def some[A](x : Js[A]) : Js[Option[A]] = Some(x)
    val none : Js[Option[Nothing]] = fromOption(None)


    implicit def fromEither[A, B](either : Either[Js[A], Js[B]]) : Js[Either[A, B]] = either match {
        case Left(x) => JsRecord[Either[A, B]](new JsObject { val tag = "Left"; val value = x })
        case Right(x) => JsRecord[Either[A, B]](new JsObject { val tag = "Right"; val value = x })
    }

    implicit def toEither[A, B](either : Js[_ <: Either[A, B]]) = new {
        def switch[R](f : PartialFunction[Either[Js[A], Js[B]], Js[R]]) : Js[R] = Let(either, { m : Js[_ <: Either[A, B]] =>
            val g : PartialFunction[Either[Js[A], Js[B]], Js[R]] = (x : Either[Js[A], Js[B]]) => x match {
                case _ => Throw[String, R]("No match")
            }
            val h : (Either[Js[A], Js[B]]) => Js[R] = f orElse g
            If(Binary(Equal, GetField(m, "tag"), "None"),
                h(Left(GetField(m, "value"))),
                h(Right(GetField(m, "value")))
            )
        })
    }

    def leftOf[A, B](x : Js[A]) : Js[Either[A, B]] = Left[Js[A], Js[B]](x)
    def rightOf[A, B](x : Js[B]) : Js[Either[A, B]] = Right[Js[A], Js[B]](x)
    def left[A](x : Js[A]) = leftOf[A, Nothing](x)
    def right[B](x : Js[B]) = rightOf[Nothing, B](x)
}
