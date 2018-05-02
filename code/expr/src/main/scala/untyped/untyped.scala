package untyped

sealed abstract class Value extends Product with Serializable
final case class IntValue(num: Int) extends Value
final case class BoolValue(bool: Boolean) extends Value

sealed abstract class Expr extends Product with Serializable
final case class Lit(v: Value) extends Expr
final case class Lt(a: Expr, b: Expr) extends Expr
final case class And(a: Expr, b: Expr) extends Expr

object Syntax {
  def lit(n: Int): Expr =
    Lit(IntValue(n))

  def lit(b: Boolean): Expr =
    Lit(BoolValue(b))

  implicit class ExprOps(val a: Expr) extends AnyVal {
    def < (b: Expr): Expr =
      Lt(a, b)

    def && (b: Expr): Expr =
      And(a, b)
  }
}

object Program {
  import Syntax._

  val program: Expr =
    lit(1) < lit(2) && lit(3) < lit(4)
}

object Interpreter {
  import cats.instances.either._
  import cats.syntax.all._

  type ErrorOr[A] = Either[String, A]

  def eval(program: Expr): ErrorOr[Value] =
    program match {
      case Lit(n) =>
        n.pure[ErrorOr]

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

object Main extends App {
  import Program._

  println("Program: " + program)

  import Interpreter._

  println("Result: " + eval(program))
}
