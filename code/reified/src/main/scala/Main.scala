sealed abstract class Expr extends Product with Serializable
final case class Add(a: Expr, b: Expr) extends Expr
final case class Multiply(a: Expr, b: Expr) extends Expr
final case class Literal(n: Int) extends Expr

object Syntax {
  implicit class ExprOps(val a: Expr) extends AnyVal {
    def + (b: Expr): Expr =
      Add(a, b)

    def * (b: Expr): Expr =
      Multiply(a, b)
  }

  def lit(n: Int): Expr =
    Literal(n)
}

object Program {
  import Syntax._

  val program1 =
    lit(1) * lit(2) + lit(3) * lit(4)
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
