package frees

import freestyle.free._

@free trait Exprs {
  def lit[A](value: A): FS[A]
  def lt(a: Int, b: Int): FS[Boolean]
  def and(a: Boolean, b: Boolean): FS[Boolean]
}

// object Syntax {
//   import cats.free.Free

//   type Expr[A] = Free[Exprs, A]

//   def lit[A](value: A): Expr[A] =
//     Free.liftF[Exprs, A](Lit(value))

//   def lt(a: Int, b: Int): Expr[Boolean] =
//     Free.liftF[Exprs, Boolean](Lt(a, b))

//   def and(a: Boolean, b: Boolean): Expr[Boolean] =
//     Free.liftF[Exprs, Boolean](And(a, b))

//   implicit class IntExprOps(val a: Expr[Int]) extends AnyVal {
//     def < (b: Expr[Int]): Expr[Boolean] =
//       a.flatMap(x => b.flatMap(y => lt(x, y)))
//   }

//   implicit class BooleanExprOps(val a: Expr[Boolean]) extends AnyVal {
//     def && (b: Expr[Boolean]): Expr[Boolean] =
//       a.flatMap(x => b.flatMap(y => and(x, y)))
//   }
// }

object Program {
  def program[F[_]](implicit exprs: Exprs[F]) = for {
    a <- exprs.lit(1)
    b <- exprs.lit(2)
    x <- exprs.lt(a, b)
    c <- exprs.lit(3)
    d <- exprs.lit(4)
    y <- exprs.lt(c, d)
    z <- exprs.and(x, y)
  } yield z
}

object Interpreter {
  import cats.Id

  implicit val idHandler: Exprs.Handler[Id] =
    new Exprs.Handler[Id] {
      override def lit[A](a: A): Id[A] =
        a

      override def lt(a: Int, b: Int): Id[Boolean] =
        a < b

      override def and(a: Boolean, b: Boolean): Id[Boolean] =
        a && b
    }

  implicit val eitherHandler: Exprs.Handler[Either[String, ?]] =
    new Exprs.Handler[Either[String, ?]] {
      override def lit[A](a: A): Either[String, A] =
        Right(a)

      override def lt(a: Int, b: Int): Either[String, Boolean] =
        if(a < b) Right(true) else Left(s"$a is not less than $b")

      override def and(a: Boolean, b: Boolean): Either[String, Boolean] =
        Right(a && b)
    }
}

object Main extends App {
  import Program._
  import Interpreter._

  import cats.Id
  import cats.instances.all._

  println("Program: " + program)
  println("Result: " + program.interpret[Id])
  println("Async result: " + program.interpret[Either[String, ?]])
}
