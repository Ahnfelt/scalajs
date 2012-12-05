package reactive

sealed abstract class Js[A] {
    def map[B](body : Js[A] => Js[B]) : Js[B] = Let(this, body)
    def flatMap[B](body : Js[A] => Js[B]) : Js[B] = Let(this, body)
}

class JsObject[A] extends Js[A]

case class Binary[A, B, C](operator : BinaryOperator[A, B, C], a : Js[A], b : Js[B]) extends Js[C]
case class Unary[A, B](operator : UnaryOperator[A, B], a : Js[A]) extends Js[B]
case class Nullary[A](operator : NullaryOperator[A]) extends Js[A]
case class If[A](condition : Js[Boolean], then : Js[A], otherwise : Js[A]) extends Js[A]
case class Recursive[A](value : Js[A] => Js[A]) extends Js[A]
case class Let[A, B](value : Js[A], body : Js[A] => Js[B]) extends Js[B]
case class Lambda[A, B](body : Js[A] => Js[B]) extends Js[A => B]
case class Apply[A, B](function : Js[A => B], argument : Js[A]) extends Js[B]
case class Tag[A](name : String) extends Js[A]
case class GetField[A, B](term : Js[A], name : String) extends Js[B]
case class JavaScript0[A](code : String) extends Js[A]
case class JavaScript1[A, B](a : Js[A], code : String => String) extends Js[B]
case class JavaScript2[A, B, C](a : Js[A], b : Js[B], code : String => String => String) extends Js[C]
case class JavaScript3[A, B, C, D](a : Js[A], b : Js[B], c : Js[C], code : String => String => String => String) extends Js[D]
case class JavaScript4[A, B, C, D, E](a : Js[A], b : Js[B], c : Js[C], d : Js[D], code : String => String => String => String => String) extends Js[E]

sealed abstract class BinaryOperator[A, B, C]
case object Add extends BinaryOperator[Double, Double, Double]
case object Subtract extends BinaryOperator[Double, Double, Double]
case object Multiply extends BinaryOperator[Double, Double, Double]
case object Divide extends BinaryOperator[Double, Double, Double]
case object Less extends BinaryOperator[Double, Double, Boolean]
case object And extends BinaryOperator[Boolean, Boolean, Boolean]
case object Or extends BinaryOperator[Boolean, Boolean, Boolean]

sealed abstract class UnaryOperator[A, B]
case object Negate extends UnaryOperator[Double, Double]
case object Not extends UnaryOperator[Boolean, Boolean]

sealed abstract class NullaryOperator[A]
case class NumberValue(value : Double) extends NullaryOperator[Double]
case class TextValue(value : String) extends NullaryOperator[String]

class FunctionTerm[A, B](term : Js[A => B]) {
    def apply(argument : Js[A]) : Js[B] = Apply(term, argument)
}

class NumberTerm(term : Js[Double]) {
    def +(that : Js[Double]) : Js[Double] = Binary(Add, term, that)
    def -(that : Js[Double]) : Js[Double] = Binary(Subtract, term, that)
    def *(that : Js[Double]) : Js[Double] = Binary(Multiply, term, that)
    def /(that : Js[Double]) : Js[Double] = Binary(Divide, term, that)
    def <(that : Js[Double]) : Js[Boolean] = Binary(Less, term, that)
}

object Js {
    def apply[A, B]
        (term : A, name : Option[String] = None)
        (implicit module : JsModule, convert : A => Js[B]) : Js[B] =
        new JsDefinition(convert(term), name, module)

    implicit def literalDouble(value : Double) = Nullary(NumberValue(value))
    implicit def literalInt(value : Int) = Nullary(NumberValue(value.toDouble))
    implicit def literalString(value : String) = Nullary(TextValue(value))
    implicit def literalFunction1[A, B](value : Js[A] => Js[B]) = Lambda(value)
    implicit def literalFunction2[A, B, C](value : Js[A] => Js[B] => Js[C]) = Lambda((a : Js[A]) => Lambda(value(a)))
    implicit def literalFunction3[A, B, C, D](value : Js[A] => Js[B] => Js[C] => Js[D]) = Lambda((a : Js[A]) => Lambda((b : Js[B]) => Lambda(value(a)(b))))
    implicit def literalFunction4[A, B, C, D, E](value : Js[A] => Js[B] => Js[C] => Js[D] => Js[E]) = Lambda((a : Js[A]) => Lambda((b : Js[B]) => Lambda((c : Js[C]) => Lambda(value(a)(b)(c)))))
    implicit def toFunction[A, B](term : Js[A => B]) = new FunctionTerm(term)
    implicit def toNumber(term : Js[Double]) = new NumberTerm(term)
    def iff[A](condition : Js[Boolean])(then : Js[A])(otherwise : Js[A]) : Js[A] = If(condition, then, otherwise)
    def recursive[A](value : Js[A] => Js[A]) : Js[A] = Recursive(value)
}

abstract class JsModule(moduleName : Option[String] = None) {
    val name : String = moduleName.getOrElse(this.getClass.getSimpleName.replace("$", ""))
    implicit val implicitModule : JsModule = this
}

case class JsDefinition[A](
    term : Js[A],
    name : Option[String],
    module : JsModule
    ) extends Js[A]

class JavaScript {

    var topLevelList = List[JsDefinition[_]]()
    var topLevelNames = new java.util.IdentityHashMap[JsDefinition[_], String]()
    var topLevel = new java.util.IdentityHashMap[JsDefinition[_], String]()

    var nextFresh = 0

    def fresh : String = {
        nextFresh += 1
        "_" + nextFresh
    }

    def apply[A](term : Js[A]) : String = {
        val t = fromScope(term)
        var is = Set[String]()
        val ds = for(d <- topLevelList) yield {
            is += d.module.name + " = {};\n"
            d.module.name + "." + topLevelNames.get(d) + " = " + topLevel.get(d) + ";\n"
        }
        is.mkString ++ ds.mkString ++ t
    }

    def fromTerm[A](term : Js[A]) : String = term match {
        case Binary(operator, a, b) => fromTerm(a) + " " + fromBinary(operator) + " " + fromTerm(b)
        case Unary(operator, a) => "-"
        case Nullary(operator) => fromNullary(operator)
        case If(condition, a, b) => "IF"
        case Let(value, body) =>
            "(function() {\n" + fromScope(term) + "\n})()\n"
        case Recursive(body) =>
            fromRecursiveFunction(body)
        case Lambda(body) =>
            val x = fresh
            "(function(" + x + ") {\n" + fromScope(body(Tag(x))) + "\n})"
        case Apply(function, argument) => fromTerm(function) + "(" + fromTerm(argument) + ")"
        case Tag(name) => name
        case definition : JsDefinition[A] =>
            if(!topLevelNames.containsKey(definition)) {
                topLevelList ::= definition
                topLevelNames.put(definition, "_" + topLevelList.size)
                topLevel.put(definition, fromTerm(definition.term))
            }
            definition.module.name + "." + topLevelNames.get(definition)
        case o : JsObject[_] =>
            "{" +
            (for(m <- o.getClass.getMethods
                if !m.getName.contains("$")
                    && m.getParameterTypes.isEmpty
                    && classOf[Js[_]].isAssignableFrom(m.getReturnType)) yield {
                "\"" + m.getName + "\": " + fromTerm(m.invoke(o).asInstanceOf[Js[_]])
            }).mkString(", ") +
            "}"
        case GetField(target, name) => fromTerm(target) + "[\"" + name + "\"]"
        case JavaScript0(code) => fromScope(term)
        case JavaScript1(a, code) => fromScope(term)
        case JavaScript2(a, b, code) => fromScope(term)
        case JavaScript3(a, b, c, code) => fromScope(term)
        case JavaScript4(a, b, c, d, code) => fromScope(term)
    }

    def ensureTag[A](term : Js[A]) : (Tag[A], String) = term match {
        case e : Tag[A] => (e, "")
        case _ =>
            val x = fresh
            (Tag(x), "var " + x + " = " + fromTerm(term) + ";\n")
    }

    def fromScope[A](term : Js[A]) : String = term match {
        case Let(value, body) =>
            val (x, c) = ensureTag(value)
            c + fromScope(body(x))
        case JavaScript0(code) =>
            code
        case JavaScript1(a, code) =>
            val (Tag(ax), as) = ensureTag(a)
            as + code(ax)
        case JavaScript2(a, b, code) =>
            val (Tag(ax), as) = ensureTag(a)
            val (Tag(bx), bs) = ensureTag(b)
            as + bs + code(ax)(bx)
        case JavaScript3(a, b, c, code) =>
            val (Tag(ax), as) = ensureTag(a)
            val (Tag(bx), bs) = ensureTag(b)
            val (Tag(cx), cs) = ensureTag(c)
            as + bs + cs + code(ax)(bx)(cx)
        case JavaScript4(a, b, c, d, code) =>
            val (Tag(ax), as) = ensureTag(a)
            val (Tag(bx), bs) = ensureTag(b)
            val (Tag(cx), cs) = ensureTag(c)
            val (Tag(dx), ds) = ensureTag(d)
            as + bs + cs + ds + code(ax)(bx)(cx)(dx)
        case _ => "return " + fromTerm(term)
    }

    def fromRecursiveFunction[A](term : Js[A]) : String = term match {
        case Lambda(body) =>
            val x = fresh
            fromRecursiveBody(body(Tag(x)), x)
    }

    def fromRecursiveBody[A](term : Js[A], x : String) : String = term match {
        case Lambda(body3) =>
            val y = fresh
            "(function " + x + "(" + y + ") {\n" + fromScope(body3(Tag(y))) + "\n})"
    }

    def fromBinary[A, B, C](term : BinaryOperator[A, B, C]) : String = term match {
        case Add => "+"
        case Subtract => "-"
        case Multiply => "*"
        case Divide => "/"
        case Less => "<"
        case And => "&&"
        case Or => "||"
    }

    def fromNullary[A](term : NullaryOperator[A]) : String = term match {
        case NumberValue(value) => value.toString
        case TextValue(value) => value.toString
    }
}

object JavaScript {
    def apply[A](term : Js[A]) : String = (new JavaScript).apply(term)
}

import Js._

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

case class Point(x : Js[Double], y : Js[Double]) extends JsObject[Point]

object Point {
    implicit def toPoint(p : Js[Point]) = Point(GetField(p, "x"), GetField(p, "y"))
}

object Example {
    def main(arguments : Array[String]) {

        val x : Js[Double] = for {
            f <- recursive[Double => Double](f => (a : Js[Double]) => a + f(42))
            y <- f(1) + FlapJax.increment(0)
            z <- y
            _ <- FlapJax.alert("foo")
            _ <- FlapJax.onClick
        } yield y

        println(JavaScript(x))

        import Point._

        println(JavaScript(for {
            p <- Point(5, 20)
            q <- p.copy(y = p.x / 2)
        } yield p.x + q.y))
    }
}
