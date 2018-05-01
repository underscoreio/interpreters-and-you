sealed abstract class Expr[A] extends Product with Serializable
final case class Infix[A, B, C](func: (A, B) => C, a: Expr[A], b: Expr[B]) extends Expr[C]
final case class Literal[A](value: A) extends Expr[A]

object Syntax {
  implicit class IntExprOps(val a: Expr[Int]) extends AnyVal {
    def + (b: Expr[Int]): Expr[Int] =
      Infix((x: Int, y: Int) => x + y, a, b)

    def * (b: Expr[Int]): Expr[Int] =
      Infix((x: Int, y: Int) => x * y, a, b)

    def < (b: Expr[Int]): Expr[Boolean] =
      Infix((x: Int, y: Int) => x < y, a, b)
  }

  implicit class BooleanExprOps(val a: Expr[Boolean]) extends AnyVal {
    def && (b: Expr[Boolean]): Expr[Boolean] =
      Infix((x: Boolean, y: Boolean) => x && y, a, b)
  }

  def lit[A](value: A): Expr[A] =
    Literal(value)
}

object Program {
  import Syntax._

  val program1 =
    lit(1) < lit(2) && lit(3) < lit(4)
}

object Interpreter {
  def eval[A](program: Expr[A]): A =
    program match {
      case Literal(v) =>
        v

      case Infix(fn, a, b) =>
        fn(eval(a), eval(b))
    }

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  def evalAsync[A](program: Expr[A]): Future[A] =
    program match {
      case Literal(v) =>
        Future.successful(v)

      case Infix(fn, a, b) =>
        for {
          a <- evalAsync(a)
          b <- evalAsync(b)
        } yield fn(a, b)
    }

  import cats.Monad
  import cats.syntax.all._

  def evalMonadic[F[_], A](program: Expr[A])(implicit m: Monad[F]): F[A] =
    program match {
      case Literal(n) =>
        n.pure[F]

      case Infix(fn, a, b) =>
        for {
          a <- evalMonadic(a)
          b <- evalMonadic(b)
        } yield fn(a, b)
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

  println("Optional result: " + evalMonadic[Option, Boolean](program1))
}
