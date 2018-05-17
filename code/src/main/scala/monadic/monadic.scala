package monadic

sealed abstract class Expr[A] extends Product with Serializable {
  def flatMap[B](func: A => Expr[B]): Expr[B] =
    FlatMap(this, func)

  def map[B](func: A => B): Expr[B] =
    FlatMap(this, (a: A) => Pure(func(a)))
}

final case class Pure[A](value: A) extends Expr[A]
final case class Lt(a: Int, b: Int) extends Expr[Boolean]
final case class And(a: Boolean, b: Boolean) extends Expr[Boolean]
final case class FlatMap[A, B](a: Expr[A], fn: A => Expr[B]) extends Expr[B]

object Syntax {
  // These "smart constructor" functions
  // construct instances of sybtypes of Expr
  // but change the return types to Expr[Foo].
  // which helps with type inference in the program below.

  def pure[A](a: A): Expr[A] =
    Pure(a)

  def lt(a: Int, b: Int): Expr[Boolean] =
    Lt(a, b)

  def and(a: Boolean, b: Boolean): Expr[Boolean] =
    And(a, b)
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
  def eval[A](program: Expr[A]): A =
    program match {
      case Pure(v) =>
        v

      case Lt(a, b) =>
        a < b

      case And(a, b) =>
        a && b

      case FlatMap(a, fn) =>
        eval(fn(eval(a)))
    }
}

// Generalising FlatMap makes the following hard to implement.
// We can't reason about the Scala function in FlatMap,
// so we can't pretty print or inspect/rewrite programs any more:

// object PrettyPrinter {
//   def print[A](program: Expr[A]): String =
//     program match {
//       case Lit(v) =>
//         v.toString
//
//       case Lt(a, b) =>
//         s"${print(a)} < ${print(b)}"
//
//       case And(a, b) =>
//         s"${print(a)} && ${print(b)}"
//
//       case FlatMap(a, f) =>
//         ???
//     }
// }

// object Simplifier {
//   def simplify[A](program: Expr[A]): Expr[A] =
//     program match {
//       case Lit(n) =>
//         ???
//
//       case Lt(a, b) =>
//         ???
//
//       case And(a, b) =>
//         ???
//
//       case FlatMap(a, f) =>
//         ???
//     }
// }

object Main extends App {
  import Program._

  println("Program: " + program)
  println("Result: " + Interpreter.eval(program))
}
