sealed abstract class ExprAlg[A]
final case class Add(a: Int, b: Int) extends ExprAlg[Int]
final case class Multiply(a: Int, b: Int) extends ExprAlg[Int]
final case class LessThan(a: Int, b: Int) extends ExprAlg[Boolean]
final case class And(a: Boolean, b: Boolean) extends ExprAlg[Boolean]
final case class Literal[A](value: A) extends ExprAlg[A]

object Syntax {
  import cats.free.Free

  type Expr[A] = Free[ExprAlg, A]

  def lit[A](value: A): Expr[A] =
    Free.liftF[ExprAlg, A](Literal(value))

  def add(a: Int, b: Int): Expr[Int] =
    Free.liftF[ExprAlg, Int](Add(a, b))

  def mul(a: Int, b: Int): Expr[Int] =
    Free.liftF[ExprAlg, Int](Multiply(a, b))

  def lt(a: Int, b: Int): Expr[Boolean] =
    Free.liftF[ExprAlg, Boolean](LessThan(a, b))

  def and(a: Boolean, b: Boolean): Expr[Boolean] =
    Free.liftF[ExprAlg, Boolean](And(a, b))

  implicit class IntExprOps(val a: Expr[Int]) extends AnyVal {
    def + (b: Expr[Int]): Expr[Int] =
      a.flatMap(x => b.flatMap(y => add(x, y)))

    def * (b: Expr[Int]): Expr[Int] =
      a.flatMap(x => b.flatMap(y => mul(x, y)))

    def < (b: Expr[Int]): Expr[Boolean] =
      a.flatMap(x => b.flatMap(y => lt(x, y)))
  }

  implicit class BooleanExprOps(val a: Expr[Boolean]) extends AnyVal {
    def && (b: Expr[Boolean]): Expr[Boolean] =
      a.flatMap(x => b.flatMap(y => and(x, y)))
  }
}

object Program {
  import Syntax._

  val program1 = for {
    a <- lit(1)
    b <- lit(2)
    c <- lit(3)
    d <- lit(4)
    x <- lt(a, b)
    y <- lt(c, d)
    z <- and(x, y)
  } yield z

  val program2 =
    lit(1) < lit(2) && lit(3) < lit(4)
}

object Interpreter {
  import cats.~>
  import cats.Monad
  import cats.syntax.all._

  def eval[F[_]](implicit m: Monad[F]): ExprAlg ~> F = new (ExprAlg ~> F) {
    def apply[A](expr: ExprAlg[A]): F[A] =
      expr match {
        case Add(a, b)      => (a + b).pure[F]
        case Multiply(a, b) => (a * b).pure[F]
        case LessThan(a, b) => (a < b).pure[F]
        case And(a, b)      => (a && b).pure[F]
        case Literal(v)     => v.pure[F]
      }
  }
}

object Main extends App {
  import Program._
  import Interpreter._

  import cats.instances.all._

  println("Program: " + program1)
  println("Result: " + program1.foldMap(eval[cats.Id]))

  import scala.concurrent.{Await, Future}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  println("Async result: " + Await.result(program1.foldMap(eval[Future]), 1.second))
}
