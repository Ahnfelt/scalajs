package scalajs

sealed abstract class Js[A] {
    def map[B](body : Js[A] => Js[B]) : Js[B] = Let(this, body)
    def flatMap[B](body : Js[A] => Js[B]) : Js[B] = Let(this, body)
}

abstract class JsObject[A] extends Js[A]

case class Binary[A, B, C](operator : BinaryOperator[A, B, C], a : Js[A], b : Js[B]) extends Js[C]
case class Unary[A, B](operator : UnaryOperator[A, B], a : Js[A]) extends Js[B]
case class Nullary[A](operator : NullaryOperator[A]) extends Js[A]
case class If[A](condition : Js[Boolean], then : Js[A], otherwise : Js[A]) extends Js[A]
case class Recursive[A](value : Js[A] => Js[A]) extends Js[A]
case class Let[A, B](value : Js[A], body : Js[A] => Js[B]) extends Js[B]
case class Lambda1[A, B](body : Js[A] => Js[B]) extends Js[A => B]
case class Lambda2[A, B, C](body : (Js[A], Js[B]) => Js[C]) extends Js[(A, B) => C]
case class Lambda3[A, B, C, D](body : (Js[A], Js[B], Js[C]) => Js[D]) extends Js[(A, B, C) => C]
case class Lambda4[A, B, C, D, E](body : (Js[A], Js[B], Js[C], Js[D]) => Js[E]) extends Js[(A, B, C, D) => E]
case class Apply1[A, B](function : Js[A => B], argument1 : Js[A]) extends Js[B]
case class Apply2[A, B, C](function : Js[(A, B) => C], argument1 : Js[A], argument2 : Js[B]) extends Js[C]
case class Apply3[A, B, C, D](function : Js[(A, B, C) => D], argument1 : Js[A], argument2 : Js[B], argument3 : Js[C]) extends Js[D]
case class Apply4[A, B, C, D, E](function : Js[(A, B, C, D) => E], argument1 : Js[A], argument2 : Js[B], argument3 : Js[C], argument4 : Js[D]) extends Js[E]

case class Tag[A](name : String) extends Js[A]
case class GetField[A](term : Js[_], name : String) extends Js[A]
case class GetIndex[A, B](term : Js[_], index : Js[_]) extends Js[A]
case class Global[A](name : String) extends Js[A]
case class Assign[A](target : Js[A], value : Js[A]) extends Js[A]
case class For(start : Js[Double], stop : Js[Double], step : Js[Double], body : Js[Double] => Js[Unit]) extends Js[Unit]
case class ArrayLiteral[A](xs : Js[A]*) extends Js[Array[A]]
case class Sequence[A](ignore : Js[_], term : Js[A]) extends Js[A]

sealed abstract class BinaryOperator[A, B, C]
case object Add extends BinaryOperator[Double, Double, Double]
case object Subtract extends BinaryOperator[Double, Double, Double]
case object Multiply extends BinaryOperator[Double, Double, Double]
case object Divide extends BinaryOperator[Double, Double, Double]
case object Equal extends BinaryOperator[String, String, Boolean]
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
    def apply(argument : Js[A]) : Js[B] = Apply1(term, argument)
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

    implicit def function1[A, B](value : Js[A] => Js[B]) = Lambda1(value)

    implicit def curriedFunction2[A, B, C](value : Js[A] => Js[B] => Js[C]) = Lambda1((a : Js[A]) => Lambda1(value(a)))
    implicit def curriedFunction3[A, B, C, D](value : Js[A] => Js[B] => Js[C] => Js[D]) = Lambda1((a : Js[A]) => Lambda1((b : Js[B]) => Lambda1(value(a)(b))))
    implicit def curriedFunction4[A, B, C, D, E](value : Js[A] => Js[B] => Js[C] => Js[D] => Js[E]) = Lambda1((a : Js[A]) => Lambda1((b : Js[B]) => Lambda1((c : Js[C]) => Lambda1(value(a)(b)(c)))))

    implicit def uncurriedFunction2[A, B, C](value : (Js[A], Js[B]) => Js[C]) = Lambda2(value)
    implicit def uncurriedFunction3[A, B, C, D](value : (Js[A], Js[B], Js[C]) => Js[D]) = Lambda3(value)
    implicit def uncurriedFunction4[A, B, C, D, E](value : (Js[A], Js[B], Js[C], Js[D]) => Js[E]) = Lambda4(value)

    implicit def toFunction1[A, B](term : Js[A => B]) = Apply1(term, _ : Js[A])
    implicit def toFunction2[A, B, C](term : Js[(A, B) => C]) = Apply2(term, _ : Js[A], _ : Js[B])
    implicit def toFunction3[A, B, C, D](term : Js[(A, B, C) => D]) = Apply3(term, _ : Js[A], _ : Js[B], _ : Js[C])
    implicit def toFunction4[A, B, C, D, E](term : Js[(A, B, C, D) => E]) = Apply4(term, _ : Js[A], _ : Js[B], _ : Js[C], _ : Js[D])

    implicit def toNumber(term : Js[Double]) = new NumberTerm(term)

    def array[A](elements : Js[A]*) = ArrayLiteral(elements : _*)
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
            case ArrayLiteral(elements @ _*) => "[" + elements.map(fromTerm).mkString(", ") + "]"
            case If(condition, a, b) => "((" + fromTerm(condition) + ") ? (" + fromTerm(a) + ") : (" + fromTerm(b) + "))"
            case Recursive(body) =>
                fromRecursiveFunction(body)
            case Lambda1(body) =>
                val x = fresh
                "(function(" + x + ") {\n" + fromScope(body(Tag(x))) + "\n})"
            case Lambda2(body) =>
                val x = fresh
                val y = fresh
                "(function(" + x + ", " + y + ") {\n" + fromScope(body(Tag(x), Tag(y))) + "\n})"
            case Lambda3(body) =>
                val x = fresh
                val y = fresh
                val z = fresh
                "(function(" + x + ", " + y + ", " + z + ") {\n" + fromScope(body(Tag(x), Tag(y), Tag(z))) + "\n})"
            case Lambda4(body) =>
                val x = fresh
                val y = fresh
                val z = fresh
                val w = fresh
                "(function(" + x + ", " + y + ", " + z + ", " + w + ") {\n" + fromScope(body(Tag(x), Tag(y), Tag(z), Tag(w))) + "\n})"
            case Apply1(function, argument1) => fromTerm(function) + "(" + fromTerm(argument1) + ")"
            case Apply2(function, argument1, argument2) => fromTerm(function) + "(" + fromTerm(argument1) + ", " + fromTerm(argument2) + ")"
            case Apply3(function, argument1, argument2, argument3) => fromTerm(function) + "(" + fromTerm(argument1) + ", " + fromTerm(argument2) + ", " + fromTerm(argument3) + ")"
            case Apply4(function, argument1, argument2, argument3, argument4) => fromTerm(function) + "(" + fromTerm(argument1) + ", " + fromTerm(argument2) + ", " + fromTerm(argument3) + ", " + fromTerm(argument4) + ")"
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
            case GetIndex(target, index) => fromTerm(target) + "[" + fromTerm(index) + "]"
            case Global(name) => name
            case Assign(target, value) => fromTerm(target) + " = " + fromTerm(value)
            case Let(_, _) => scoped(term)
            case For(_, _, _, _) => scoped(term)
            case Sequence(_, _) => scoped(term)
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
        case Sequence(a, b) =>
            fromScope(a, false) + ";\n" + fromScope(b, returns)
        case Let(value, body) =>
            val (x, c) = ensureTag(value)
            c + fromScope(body(x))
        case For(start, stop, step, body) =>
            val x = fresh
            "for(var " + x + " = " + fromTerm(start) + "; " + x + " < " + fromTerm(stop) + "; " + x + " += " ++ fromTerm(step) + ") {\n" +
            fromScope(body(Tag(x)), false) + "\n" +
            "}"
        case _ => (if(returns) "return " else "") + fromTerm(term)
    }

    def fromRecursiveFunction[A](term : Js[A]) : String = term match {
        case Lambda1(body) =>
            val x = fresh
            fromRecursiveBody(body(Tag(x)), x)
        case _ => throw new IllegalArgumentException
    }

    def fromRecursiveBody[A](term : Js[A], x : String) : String = term match {
        case Lambda1(body3) =>
            val y = fresh
            "(function " + x + "(" + y + ") {\n" + fromScope(body3(Tag(y))) + "\n})"
        case _ => throw new IllegalArgumentException
    }

    def fromBinary[A, B, C](term : BinaryOperator[A, B, C]) : String = term match {
        case Add => "+"
        case Subtract => "-"
        case Multiply => "*"
        case Divide => "/"
        case Equal => "==="
        case Less => "<"
        case And => "&&"
        case Or => "||"
    }

    def fromNullary[A](term : NullaryOperator[A]) : String = term match {
        case Null => "null"
        case NumberValue(value) => value.toString
        case TextValue(value) => "\"" + value.toString.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    }
}

object JavaScript {
    def apply[A](term : Js[A]) : String = (new JavaScript).apply(term)
}
