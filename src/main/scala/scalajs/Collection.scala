package scalajs

object Collection extends JsModule {
    val range = Js { a : Js[Double] => b : Js[Double] => for {
        array <- Imperative.Array[Double]()
        _ <- Imperative.For(a, b, 1, { i =>
            GetField[Array[Double], Double => Unit](array, "push")(i)
        })
    } yield array}

    case class JsArray[A](term : Js[Array[A]]) extends JsObject[Array[A]] {
        def each[B](f : Js[A => B]) : Js[Array[B]] = Apply[(A => B), Array[B]](GetField(term, "map"), f)
    }

    implicit def toList[A](term : Js[Array[A]]) : JsArray[A] = JsArray(term)
}

