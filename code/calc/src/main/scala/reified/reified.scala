package reified

sealed abstract class Expr extends Product with Serializable
final case class Lit(n: Int) extends Expr
final case class Add(a: Expr, b: Expr) extends Expr
final case class Mul(a: Expr, b: Expr) extends Expr

object Syntax {
  def lit(n: Int): Expr =
    Lit(n)

  implicit class ExprOps(val a: Expr) extends AnyVal {
    def + (b: Expr): Expr =
      Add(a, b)

    def * (b: Expr): Expr =
      Mul(a, b)
  }
}

object Program {
  import Syntax._

  val program1: Expr =
    lit(1) * lit(2) + lit(3) * lit(4)

  val program2: Expr =
    (lit(1) + lit(2)) * (lit(3) + lit(4))
}

object Interpreter {
  def eval(program: Expr): Int =
    program match {
      case Lit(n)    => n
      case Add(a, b) => eval(a) + eval(b)
      case Mul(a, b) => eval(a) * eval(b)
    }
}

object AsyncInterpreter {
  import scala.concurrent.{ExecutionContext, Future}

  def eval(program: Expr)(implicit ec: ExecutionContext): Future[Int] =
    program match {
      case Lit(n) =>
        Future.successful(n)

      case Add(a, b) =>
        for {
          a <- eval(a)
          b <- eval(b)
        } yield a + b

      case Mul(a, b) =>
        for {
          a <- eval(a)
          b <- eval(b)
        } yield a * b
    }
}

object GeneralInterpreter {
  import cats.Monad
  import cats.syntax.all._

  def eval[F[_]](program: Expr)(implicit m: Monad[F]): F[Int] =
    program match {
      case Lit(n) =>
        n.pure[F]

      case Add(a, b) =>
        (eval(a), eval(b)).mapN(_ + _)

      case Mul(a, b) =>
        (eval(a), eval(b)).mapN(_ * _)
    }
}

object PrettyPrinter {
  def print(program: Expr): String =
    program match {
      case Lit(n) =>
        s"$n"

      case Add(a, b) =>
        s"${print(a)} + ${print(b)}"

      case Mul(a, b) =>
        s"${print(a)} * ${print(b)}"
    }
}

object Simplifier {
  def simplify(program: Expr): Expr =
    program match {
      case Lit(n) =>
        Lit(n)

      case Add(a, b) =>
        Add(simplify(a), simplify(b))

      case Mul(Add(a, b), c) =>
        Add(simplify(Mul(a, c)), simplify(Mul(b, c)))

      case Mul(a, Add(b, c)) =>
        Add(simplify(Mul(a, b)), simplify(Mul(a, c)))

      case Mul(a, b) =>
        Mul(simplify(a), simplify(b))
    }
}

object Main extends App {
  import Program._

  println("Program: " + program1)
  println("Result: " + Interpreter.eval(program1))
  println("Printed: " + PrettyPrinter.print(program1))

  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  println("Async result: " + Await.result(AsyncInterpreter.eval(program1), 1.second))

  import cats.instances.all._

  println("Optional result: " + GeneralInterpreter.eval[Option](program1))

  println("Simplified: " + Simplifier.simplify(program2))
}
