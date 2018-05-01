sealed abstract class Expr extends Product with Serializable
final case class Add(a: Expr, b: Expr) extends Expr
final case class Multiply(a: Expr, b: Expr) extends Expr
final case class Literal(n: Int) extends Expr

object Parser {
  import fastparse.all._

  def parse(text: String): Expr =
    expr.parse(text).get.value

  val expr: Parser[Expr] =
    P(add ~ End)

  val add: Parser[Expr] =
    P(mul ~ (ws ~ "+" ~ ws ~ mul).rep)
      .map { case (head, tail) => tail.foldLeft(head)(Add) }

  val mul: Parser[Expr] =
    P(term ~ (ws ~ "*" ~ ws ~ term).rep)
      .map { case (head, tail) => tail.foldLeft(head)(Multiply) }

  val term: Parser[Expr] =
    P(paren | lit)

  val paren: Parser[Expr] =
    P("(" ~ ws ~ add ~ ws ~ ")")

  val lit: Parser[Expr] =
    P(CharIn("+-").? ~ CharIn('0' to '9').rep(1)).!
      .map(_.toInt).map(Literal)

  val ws: Parser[Unit] =
    P(CharIn(" \t").rep)
}

object Program {
  import Parser.parse

  val program1 =
    parse("""1 * 2 + 3 * 4""")
}

object Interpreter {
  def eval(program: Expr): Int =
    program match {
      case Literal(n)     => n
      case Add(a, b)      => eval(a) + eval(b)
      case Multiply(a, b) => eval(a) * eval(b)
    }

  import scala.concurrent.{Future, ExecutionContext}

  def evalAsync(program: Expr)(implicit ec: ExecutionContext): Future[Int] =
    program match {
      case Literal(n) =>
        Future.successful(n)

      case Add(a, b) =>
        for {
          a <- evalAsync(a)
          b <- evalAsync(b)
        } yield a + b

      case Multiply(a, b) =>
        for {
          a <- evalAsync(a)
          b <- evalAsync(b)
        } yield a * b
    }

  import cats.Monad
  import cats.syntax.all._

  def evalMonadic[F[_]](program: Expr)(implicit m: Monad[F]): F[Int] =
    program match {
      case Literal(n) =>
        n.pure[F]

      case Add(a, b) =>
        for {
          a <- evalMonadic(a)
          b <- evalMonadic(b)
        } yield a + b

      case Multiply(a, b) =>
        for {
          a <- evalMonadic(a)
          b <- evalMonadic(b)
        } yield a * b
    }
}

object Main extends App {
  import Program._

  println("Program: " + program1)

  import Interpreter._

  println("Result: " + eval(program1))

  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  println("Async result: " + Await.result(evalAsync(program1), 1.second))

  import cats.instances.all._

  println("Optional result: " + evalMonadic[Option](program1))
}
