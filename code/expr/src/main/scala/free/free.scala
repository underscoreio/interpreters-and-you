package free

sealed abstract class ExprAlg[A]
final case class Lit[A](value: A) extends ExprAlg[A]
final case class Lt(a: Int, b: Int) extends ExprAlg[Boolean]
final case class And(a: Boolean, b: Boolean) extends ExprAlg[Boolean]

object Syntax {
  import cats.free.Free

  type Expr[A] = Free[ExprAlg, A]

  def lit[A](value: A): Expr[A] =
    Free.liftF[ExprAlg, A](Lit(value))

  def lt(a: Int, b: Int): Expr[Boolean] =
    Free.liftF[ExprAlg, Boolean](Lt(a, b))

  def and(a: Boolean, b: Boolean): Expr[Boolean] =
    Free.liftF[ExprAlg, Boolean](And(a, b))

  implicit class IntExprOps(val a: Expr[Int]) extends AnyVal {
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
        case Lit(v)    => v.pure[F]
        case Lt(a, b)  => (a < b).pure[F]
        case And(a, b) => (a && b).pure[F]
      }
  }
}

object PrettyPrinter {
  import cats.~>
  import cats.data.Writer
  import cats.syntax.all._

  type Printed[A] = Writer[List[String], A]

  def print: ExprAlg ~> Printed = new (ExprAlg ~> Printed) {
    def apply[A](expr: ExprAlg[A]): Printed[A] =
      expr match {
        case Lit(v)    => v.writer(List(v.toString))
        case Lt(a, b)  => (a < b).writer(List("<"))
        case And(a, b) => (a && b).writer(List("&&"))
      }
  }
}

object Main extends App {
  import Program._

  import cats.Id
  import cats.instances.all._
  import scala.concurrent.{Await, Future}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  println("Program: " + program1)
  println("Result: " + program1.foldMap(Interpreter.eval[Id]))
  println("Async result: " + Await.result(program1.foldMap(Interpreter.eval[Future]), 1.second))
  println("Printed 1: " + program1.foldMap(PrettyPrinter.print).written)
  println("Printed 2: " + program2.foldMap(PrettyPrinter.print).written)
}
