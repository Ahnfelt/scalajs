package scalajs

object Pattern {
    case class Maybe[T](tag : Js[String], value : Js[T]) extends JsObject[Option[T]]
    implicit def fromOption[T](option : Option[Js[T]]) : Maybe[T] = option match {
        case Some(x) => Maybe("Some", x)
        case None => Maybe("None", null)
    }
    implicit def toMaybe[T](option : Js[Option[T]]) : Maybe[T] = {
        Maybe[T](GetField(option, "tag"), GetField(option, "value"))
    }
    implicit def toOption[T](option : Js[Option[T]]) = new {
        def switch[R](f : Option[Js[T]] => Js[R]) : Js[R] = {
            If(Binary(Equal, GetField(option, "tag"), "None"),
                f(None),
                f(Some(GetField(option, "value")))
            )
        }
    }
}
