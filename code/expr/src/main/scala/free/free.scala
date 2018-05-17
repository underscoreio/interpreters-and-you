package free

sealed abstract class ExprAlg[A]
final case class Lt(a: Int, b: Int) extends ExprAlg[Boolean]
final case class And(a: Boolean, b: Boolean) extends ExprAlg[Boolean]

object Syntax {
  import cats.free.Free

  type Expr[A] = Free[ExprAlg, A]

  def pure[A](value: A): Expr[A] =
    Free.pure[ExprAlg, A](value)

  def lt(a: Int, b: Int): Expr[Boolean] =
    Free.liftF[ExprAlg, Boolean](Lt(a, b))

  def and(a: Boolean, b: Boolean): Expr[Boolean] =
    Free.liftF[ExprAlg, Boolean](And(a, b))
}

object Program {
  import Syntax._

  val program: Expr[Boolean] =
    for {
      a <- pure(1)
      b <- pure(2)
      x <- lt(a, b)
      c <- pure(3)
      d <- pure(4)
      y <- lt(c, d)
      z <- and(x, y)
    } yield z
}

object Interpreter {
  import cats.~>
  import cats.Monad
  import cats.syntax.all._

  def eval[F[_]](implicit m: Monad[F]): ExprAlg ~> F = new (ExprAlg ~> F) {
    def apply[A](expr: ExprAlg[A]): F[A] =
      expr match {
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

  println("Program: " + program)
  println("Result: " + program.foldMap(Interpreter.eval[Id]))
  println("Async result: " + Await.result(program.foldMap(Interpreter.eval[Future]), 1.second))
  println("Printed 1: " + program.foldMap(PrettyPrinter.print).written)
}
