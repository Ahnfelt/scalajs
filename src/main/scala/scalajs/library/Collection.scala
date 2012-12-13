package scalajs.library

import scalajs._
import scalajs.Js._
import scalajs.Terms._ // TODO: Get rid of this and make Terms private

object Collection extends JsModule {

    // Arrays

    val range : Js[(Double, Double) => Array[Double]] = Js { (a : Js[Double], b : Js[Double]) => for {
        array <- array[Double]()
        _ <- For(a, b + 1, 1, { i =>
            Apply1(GetField[Double => Unit](array, "push"), i)
        })
    } yield array}

    implicit def toArray[A](term : Js[Array[A]]) = new {
        def each[B](f : Js[A => B]) : Js[Unit] = Apply1[A => B, Unit](GetField(term, "forEach"), f)
        def select[B](f : Js[A => B]) : Js[Array[B]] = Apply1[A => B, Array[B]](GetField(term, "map"), f)
        def where(f : Js[A => Boolean]) : Js[Array[A]] = Apply1[A => Boolean, Array[A]](GetField(term, "filter"), f)
        def foldLeft[B](f : Js[(B, A) => B], x : Js[B]) : Js[Array[B]] = Apply2[(B, A) => B, B, Array[B]](GetField(term, "reduce"), f, x)
        def foldRight[B](f : Js[(A, B) => B], x : Js[B]) : Js[Array[B]] = Apply2[(A, B) => B, B, Array[B]](GetField(term, "reduceRight"), f, x)
        def all(f : Js[A => Boolean]) : Js[Boolean] = Apply1[A => Boolean, Boolean](GetField(term, "every"), f)
        def any(f : Js[A => Boolean]) : Js[Boolean] = Apply1[A => Boolean, Boolean](GetField(term, "some"), f)
        def length : Js[Double] = GetField(term, "length")
        def apply(i : Js[Double]) : Js[A] = GetIndex(term, i)
        def switch[B](caseEmpty : Js[B], caseNonEmpty : Js[Array[A]] => Js[B]) : Js[B] = If(GetField(term, "length"), caseNonEmpty(term), caseEmpty)
    }

}

