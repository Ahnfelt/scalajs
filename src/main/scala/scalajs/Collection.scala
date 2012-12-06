package scalajs

object Collection extends JsModule {
    val range = Js { (a : Js[Double], b : Js[Double]) => for {
        array <- Imperative.Array[Double]()
        _ <- Imperative.For(a, b, 1, { i =>
            GetField[Array[Double], Double => Unit](array, "push")(i)
        })
    } yield array}

    case class JsArray[A](term : Js[Array[A]]) extends JsObject[Array[A]] {
        def select[B](f : Js[A => B]) : Js[Array[B]] = Apply1[A => B, Array[B]](GetField(term, "map"), f)
        def where(f : Js[A => Boolean]) : Js[Array[A]] = Apply1[A => Boolean, Array[A]](GetField(term, "filter"), f)
        def foldLeft[B](f : Js[(B, A) => B], x : Js[B]) : Js[Array[B]] = Apply2[(B, A) => B, B, Array[B]](GetField(term, "reduce"), f, x)
        def foldRight[B](f : Js[(A, B) => B], x : Js[B]) : Js[Array[B]] = Apply2[(A, B) => B, B, Array[B]](GetField(term, "reduceRight"), f, x)
        def all(f : Js[A => Boolean]) : Js[Boolean] = Apply1[A => Boolean, Boolean](GetField(term, "every"), f)
        def any(f : Js[A => Boolean]) : Js[Boolean] = Apply1[A => Boolean, Boolean](GetField(term, "some"), f)
    }

    implicit def toArray[A](term : Js[Array[A]]) : JsArray[A] = JsArray(term)
}

