package standalone

sealed abstract class Value extends Product with Serializable
final case class IntValue(num: Int) extends Value
final case class BoolValue(bool: Boolean) extends Value

sealed abstract class Expr extends Product with Serializable
final case class Lit(v: Value) extends Expr
final case class Lt(a: Expr, b: Expr) extends Expr
final case class And(a: Expr, b: Expr) extends Expr

//noinspection ForwardReference
object Parser {
  import fastparse.all._

  def parse(text: String): Expr =
    expr.parse(text).get.value

  private val expr: Parser[Expr] =
    P(and ~ End)

  private val and: Parser[Expr] =
    P(lt ~ (ws ~ "&&" ~ ws ~ lt).rep)
      .map { case (head, tail) => tail.foldLeft(head)(And) }

  private val lt: Parser[Expr] =
    P(term ~ (ws ~ "<" ~ ws ~ term).rep)
      .map { case (head, tail) => tail.foldLeft(head)(Lt) }

  private val term: Parser[Expr] =
    P(paren | lit)

  private val paren: Parser[Expr] =
    P("(" ~ ws ~ and ~ ws ~ ")")

  private val lit: Parser[Expr] =
    P(CharIn("+-").? ~ CharIn('0' to '9').rep(1)).!
      .map(_.toInt).map(IntValue).map(Lit)

  private val ws: Parser[Unit] =
    P(CharIn(" \t").rep)
}

object Program {
  import Parser.parse

  val program: Expr =
    parse("""1 < 2 && 3 < 4""")
}

object Interpreter {
  import cats.instances.either._
  import cats.syntax.all._ // for pure and flatMap

  type ErrorOr[A] = Either[String, A]

  def eval(program: Expr): ErrorOr[Value] =
    program match {
      case Lit(value) =>
        value.pure[ErrorOr]

      case Lt(a, b) =>
        (evalAsInt(a), evalAsInt(b)).mapN((a, b) => BoolValue(a < b))

      case And(a, b) =>
        (evalAsBool(a), evalAsBool(b)).mapN((a, b) => BoolValue(a && b))
    }

  def evalAsInt(expr: Expr): ErrorOr[Int] =
    eval(expr).flatMap {
      case IntValue(num)   => Right(num)
      case BoolValue(bool) => Left(s"Expected int, found $bool")
    }

  def evalAsBool(expr: Expr): ErrorOr[Boolean] =
    eval(expr).flatMap {
      case IntValue(num)   => Left(s"Expected boolean, found $num")
      case BoolValue(bool) => Right(bool)
    }
}

object AsyncInterpreter {
  import scala.concurrent.{Future, ExecutionContext}
  import cats.data.EitherT
  import cats.instances.all._
  import cats.syntax.all._ // for pure and flatMap

  // This interpreter uses a monad transformer called EitherT
  // to provided a convenient way of flatMapping
  // over an Either nested inside a Future.
  //
  // For more information on this, see Chapters 4 and 5 of "Scala with Cats":
  // https://underscore.io/books/scala-with-cats

  type ErrorOr[A] = EitherT[Future, String, A]

  def eval(program: Expr)(implicit ec: ExecutionContext): ErrorOr[Value] =
    program match {
      case Lit(v) =>
        v.pure[ErrorOr]

      case Lt(a, b) =>
        val x = evalAsInt(a)
        val y = evalAsInt(b)
        x.flatMap(a => y.map(b => BoolValue(a < b)))

      case And(a, b) =>
        val x = evalAsBool(a)
        val y = evalAsBool(b)
        x.flatMap(a => y.map(b => BoolValue(a && b)))
    }

  def evalAsInt(expr: Expr)(implicit ec: ExecutionContext): ErrorOr[Int] =
    eval(expr).flatMap {
      case IntValue(num)   => num.pure[ErrorOr]
      case BoolValue(bool) => s"Expected int, found $bool".raiseError[ErrorOr, Int]
    }

  def evalAsBool(expr: Expr)(implicit ec: ExecutionContext): ErrorOr[Boolean] =
    eval(expr).flatMap {
      case IntValue(num)   => s"Expected boolean, found $num".raiseError[ErrorOr, Boolean]
      case BoolValue(bool) => bool.pure[ErrorOr]
    }
}

object PrettyPrinter {
  def print(program: Expr): String =
    program match {
      case Lit(v) =>
        v.toString

      case Lt(a, b) =>
        s"${print(a)} < ${print(b)}"

      case And(a, b) =>
        s"${print(a)} && ${print(b)}"
    }
}

object Simplifier {
  def simplify(program: Expr): Expr =
    program match {
      case Lit(n) =>
        Lit(n)

      case Lt(Lit(IntValue(a)), Lit(IntValue(b))) =>
        Lit(BoolValue(a < b))

      case Lt(a, b) =>
        Lt(simplify(a), simplify(b))

      case And(a, b) =>
        And(simplify(a), simplify(b))
    }
}

object Main extends App {
  import Program._

  import scala.concurrent.ExecutionContext.Implicits.global

  println("Program: " + program)
  println("Result: " + Interpreter.eval(program))
  println("Async result: " + AsyncInterpreter.eval(program))
  println("Printed: " + PrettyPrinter.print(program))
}
