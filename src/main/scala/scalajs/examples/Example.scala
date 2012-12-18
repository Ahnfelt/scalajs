package scalajs.examples

import scalajs.Js._
import scalajs.{JavaScript, Js}
import scalajs.library.FlapJax
import scalajs.library.Pattern._
import scalajs.library.Geometry._
import scalajs.library.Collection._

object Example {
    def main(arguments : scala.Array[String]) {

        val test1 : Js[Double] = for {
            f <- recursive[Double => Double](f => (a : Js[Double]) => a + f(42))
            y <- f(1) + FlapJax.increment(0)
            z <- y
            _ <- FlapJax.alert("foo")
            _ <- FlapJax.onClick
        } yield y

        println(JavaScript(test1))

        val test2 = for {
            p <- Point(5, 20)
            q <- p.copy(y = p.x / 2)
        } yield p.x + q.y

        println()
        println(JavaScript(test2))


        val test3 = for {
            p <- range(0, 10)
            f <- (x : Js[Double]) => x * 2
            x <- p.select(f)
            y <- p.foldLeft[Double]((a : Js[Double], b : Js[Double]) => a + b, 0)
            z <- p(34)
            w <- p.switch(0, a => a.length * 2)
        } yield y

        println()
        println(JavaScript(test3))

        val test4 = for {
            a <- some(42)
            b <- a switch {
                case Some(x) => x + 2
                case None => 0
            }
            c <- left(7)
            d <- c switch {
                case Left(x) => x
                case Right(x) => 0
            }
        } yield d

        println()
        println(JavaScript(test4))
    }
}
