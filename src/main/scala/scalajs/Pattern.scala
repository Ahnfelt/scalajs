package scalajs

object Pattern {

    class Maybe[T](tag : Js[String]) extends JsObject[Maybe[T]]
    case class Empty[T] extends Maybe[T]("Empty")
    case class Full[T](value : Js[T]) extends Maybe[T]("Full")

    implicit def toMaybe[T](maybe : Js[Maybe[T]]) = new {
        def switch[R](f : Js[Maybe[T]] => Js[R]) : Js[R] = Let(maybe, { m : Js[Maybe[T]] =>
            If(Binary(Equal, GetField(m, "tag"), "Empty"),
                f(Empty[T]),
                f(Full(GetField(m, "value")))
            )
        })
    }
}
