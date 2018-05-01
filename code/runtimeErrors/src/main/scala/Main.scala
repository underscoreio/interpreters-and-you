sealed abstract class Expr extends Product with Serializable
final case class Add(a: Expr, b: Expr) extends Expr
final case class Multiply(a: Expr, b: Expr) extends Expr
final case class LessThan(a: Expr, b: Expr) extends Expr
final case class And(a: Expr, b: Expr) extends Expr
final case class Literal(value: Value) extends Expr

sealed abstract class Value extends Product with Serializable
final case class IntValue(n: Int) extends Value
final case class BoolValue(b: Boolean) extends Value

object Syntax {
  implicit class IntExprOps(val a: Expr) extends AnyVal {
    def + (b: Expr): Expr =
      Add(a, b)

    def * (b: Expr): Expr =
      Multiply(a, b)

    def < (b: Expr): Expr =
      LessThan(a, b)

    def && (b: Expr): Expr =
      And(a, b)
  }

  def lit(n: Int): Expr =
    Literal(IntValue(n))

  def lit(b: Boolean): Expr =
    Literal(BoolValue(b))
}

object Program {
  import Syntax._

  val program1 =
    lit(1) < lit(2) && lit(3) < lit(4)
}

object Interpreter {
  def eval(program: Expr): Either[String, Value] =
    program match {
      case Literal(v) =>
        Right(v)

      case Add(a, b) =>
        for {
          a <- eval(a).flatMap(toInt)
          b <- eval(b).flatMap(toInt)
        } yield IntValue(a + b)

      case Multiply(a, b) =>
        for {
          a <- eval(a).flatMap(toInt)
          b <- eval(b).flatMap(toInt)
        } yield IntValue(a * b)

      case LessThan(a, b) =>
        for {
          a <- eval(a).flatMap(toInt)
          b <- eval(b).flatMap(toInt)
        } yield BoolValue(a < b)

      case And(a, b) =>
        for {
          a <- eval(a).flatMap(toBool)
          b <- eval(b).flatMap(toBool)
        } yield BoolValue(a && b)
    }

  def toInt(value: Value): Either[String, Int] =
    value match {
      case IntValue(n)  => Right(n)
      case BoolValue(_) => Left("Expected int, received boolean")
    }

  def toBool(value: Value): Either[String, Boolean] =
    value match {
      case BoolValue(b) => Right(b)
      case IntValue(_)  => Left("Expected boolean, received int")
    }
}

object Main extends App {
  import Program._

  println("Program: " + program1)

  import Interpreter._

  println("Result: " + eval(program1))
}
