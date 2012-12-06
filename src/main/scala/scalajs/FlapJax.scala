package scalajs

import scalajs.Js._

// TODO: Make this about FlapJax instead of random stuff ...

object FlapJax extends JsModule {

    val alert = Js { a : Js[String] =>
        JavaScript1[String, Double](a, x => "window.alert(" + x + ")")
    }

    val pi = Js { 3.141593 }

    val increment : Js[Double => Double] = Js { x : Js[Double] =>
        x + increment(pi)
    }

    val greatest = Js { x : Js[Double] => y : Js[Double] =>
        iff(x < y) { y } { x }
    }

    val onClick = Js { x : Js[Double] => y : Js[Double] => for {
        z <- x * y
        q <- z + z
    } yield q }
}
