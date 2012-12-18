package scalajs.library

import scalajs.Js._
import scalajs.{Js, JsModule}

// TODO: Get rid of this and make Terms private

object Collection extends JsModule {

    // Arrays

    val range : Js[(Double, Double) => Array[Double]] = Js { (a : Js[Double], b : Js[Double]) => for {
        array <- array[Double]()
        _ <- forLoop(a, b + 1, 1, { i =>
            (array ! "push" : Js[Double => Unit])(i)
        })
    } yield array}

    implicit def toArray[A](term : Js[Array[A]]) = new {
        def each[B](f : Js[A => B]) : Js[Unit] = (term ! "forEach" : Js[(A => B) => Unit])(f)
        def select[B](f : Js[A => B]) : Js[Array[B]] = (term ! "map" : Js[(A => B) => Array[B]])(f)
        def where(f : Js[A => Boolean]) : Js[Array[A]] = (term ! "filter" : Js[(A => Boolean) => Array[A]])(f)
        def foldLeft[B](f : Js[(B, A) => B], x : Js[B]) : Js[Array[B]] = (term ! "reduce" : Js[((B, A) => B, B) => Array[B]])(f, x)
        def foldRight[B](f : Js[(A, B) => B], x : Js[B]) : Js[Array[B]] = (term ! "reduceRight" : Js[((A, B) => B, B) => Array[B]])(f, x)
        def all(f : Js[A => Boolean]) : Js[Boolean] = (term ! "every" : Js[(A => Boolean) => Boolean])(f)
        def any(f : Js[A => Boolean]) : Js[Boolean] = (term ! "some" : Js[(A => Boolean) => Boolean])(f)
        def length : Js[Double] = term ! "length"
        def apply(i : Js[Double]) : Js[A] = term !! i
        def switch[B](caseEmpty : Js[B], caseNonEmpty : Js[Array[A]] => Js[B]) : Js[B] = iff(term ! "length")(caseNonEmpty(term))(caseEmpty)
    }

}

