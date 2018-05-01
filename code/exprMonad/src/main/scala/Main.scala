sealed abstract class Expr[A] extends Product with Serializable
final case class Pure[A](value: A) extends Expr[A]
final case class FlatMap[A, B](expr: Expr[A], func: A => Expr[B]) extends Expr[B]

object Syntax {
  implicit class AnyExprOps[A](val expr: Expr[A]) extends AnyVal {
    def flatMap[B](func: A => Expr[B]): Expr[B] =
      FlatMap[A, B](expr, func)

    def map[B](func: A => B): Expr[B] =
      FlatMap[A, B](expr, a => Pure(func(a)))
  }

  implicit class IntExprOps(val a: Expr[Int]) extends AnyVal {
    def + (b: Expr[Int]): Expr[Int] =
      a.flatMap(x => b.map(y => x + y))

    def * (b: Expr[Int]): Expr[Int] =
      a.flatMap(x => b.map(y => x * y))

    def < (b: Expr[Int]): Expr[Boolean] =
      a.flatMap(x => b.map(y => x < y))
  }

  implicit class BooleanExprOps(val a: Expr[Boolean]) extends AnyVal {
    def && (b: Expr[Boolean]): Expr[Boolean] =
      a.flatMap(x => b.map(y => x && y))
  }

  def lit[A](value: A): Expr[A] =
    Pure(value)
}

object Program {
  import Syntax._

  val program1 = for {
    a <- lit(1)
    b <- lit(2)
    c <- lit(3)
    d <- lit(4)
  } yield a < b && c < d

  val program2 =
    lit(1) < lit(2) && lit(3) < lit(4)
}

object Interpreter {
  def eval[A](program: Expr[A]): A =
    program match {
      case Pure(v) =>
        v

      case FlatMap(a, f) =>
        eval(f(eval(a)))
    }

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  def evalAsync[A](program: Expr[A]): Future[A] =
    program match {
      case Pure(v) =>
        Future.successful(v)

      case FlatMap(a, f) =>
        evalAsync(a).flatMap(x => evalAsync(f(x)))
    }

  import cats.Monad
  import cats.syntax.all._

  def evalMonadic[F[_], A](program: Expr[A])(implicit m: Monad[F]): F[A] =
    program match {
      case Pure(v) =>
        v.pure[F]

      case FlatMap(a, f) =>
        evalMonadic(a).flatMap(x => evalMonadic(f(x)))
    }
}

object Main extends App {
  import Program._

  println("Program: " + program2)

  import Interpreter._

  println("Result: " + eval(program2))

  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  println("Async result: " + Await.result(evalAsync(program2), 1.second))

  import cats.instances.all._

  println("Monadic result: " + evalMonadic[Option, Boolean](program2))
}
