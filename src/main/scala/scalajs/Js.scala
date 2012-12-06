package scalajs

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

sealed abstract class Imperative[A]
object Imperative {
    case class For(start : Js[Double], stop : Js[Double], step : Js[Double], body : Js[Double] => Js[Unit]) extends Js[Unit]
    case class Array[A](xs : Js[A]*) extends Js[scala.Array[A]]
}

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
case object Null extends NullaryOperator[Unit]
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

    implicit def literalUnit(value : Unit) : Js[Unit] = Nullary(Null)
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

// TODO: Is this the right treatment of continuations? See http://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
case class JsAsync[R, A](run : (A => R) => R) {
    def map[B](f : A => JsAsync[R, B]) : JsAsync[R, B] = JsAsync(k => run(a => f(a).run(k)))
}

object JsAsync {
    def constant[R, A](n : A) : JsAsync[R, A] = JsAsync((k : A => R) => k(n))
}

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

    def fromTerm[A](term : Js[A]) : String = {
        term match {
            case Binary(operator, a, b) => fromTerm(a) + " " + fromBinary(operator) + " " + fromTerm(b)
            case Unary(operator, a) => "-"
            case Nullary(operator) => fromNullary(operator)
            case Imperative.Array(elements @ _*) => "[" + elements.map(fromTerm).mkString(", ") + "]"
            case If(condition, a, b) => "IF"
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
                         if !m.getName.contains("$") && m.getDeclaringClass != classOf[JsObject[_]]
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
            case Let(_, _) => scoped(term)
            case Imperative.For(_, _, _, _) => scoped(term)
        }
    }

    def ensureTag[A](term : Js[A]) : (Tag[A], String) = term match {
        case e : Tag[A] => (e, "")
        case _ =>
            val x = fresh
            (Tag(x), "var " + x + " = " + fromTerm(term) + ";\n")
    }

    def scoped[A](term : Js[A]) : String = "(function() {\n" + fromScope(term) + "\n})()"

    def fromScope[A](term : Js[A], returns : Boolean = true) : String = term match {
        case Let(value, body) =>
            val (x, c) = ensureTag(value)
            c + fromScope(body(x))
        case Imperative.For(start, stop, step, body) =>
            val x = fresh
            "for(var " + x + " = " + fromTerm(start) + "; x < " + fromTerm(stop) + "; x += " ++ fromTerm(step) + ") {\n" +
            fromScope(body(Tag(x)), false) + "\n" +
            "}"
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
        case _ => (if(returns) "return " else "") + fromTerm(term)
    }

    def fromRecursiveFunction[A](term : Js[A]) : String = term match {
        case Lambda(body) =>
            val x = fresh
            fromRecursiveBody(body(Tag(x)), x)
        case _ => throw new IllegalArgumentException
    }

    def fromRecursiveBody[A](term : Js[A], x : String) : String = term match {
        case Lambda(body3) =>
            val y = fresh
            "(function " + x + "(" + y + ") {\n" + fromScope(body3(Tag(y))) + "\n})"
        case _ => throw new IllegalArgumentException
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
        case Null => "null"
        case NumberValue(value) => value.toString
        case TextValue(value) => value.toString
    }
}

object JavaScript {
    def apply[A](term : Js[A]) : String = (new JavaScript).apply(term)
}
