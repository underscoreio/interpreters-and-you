import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

case class ExprAlg[A](value: A)

object Syntax {
  import cats.free.Free

  type Expr[A] = Free[ExprAlg, A]

  def lit[A](value: A): Expr[A] =
    Free.liftF[ExprAlg, A](ExprAlg(value))

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
  import cats.~>
  import cats.Monad
  import cats.syntax.all._

  def eval[F[_]](implicit m: Monad[F]): ExprAlg ~> F = new (ExprAlg ~> F) {
    def apply[A](expr: ExprAlg[A]): F[A] =
      expr.value.pure[F]
  }
}

object Main extends App {
  import Program._
  import Interpreter._

  import cats.instances.all._

  println("Program: " + program2)
  println("Result: " + program2.foldMap(eval[cats.Id]))
  println("Async result: " + program2.foldMap(eval[Future]))
}
