package frees

import freestyle.free._

@free trait ExprAlg {
  def lit[A](value: A): FS[A]

  def lt(a: Int, b: Int): FS[Boolean]

  def and(a: Boolean, b: Boolean): FS[Boolean]
}

object Program {
  def program[F[_]](implicit expr: ExprAlg[F]) = {
    import expr._

    for {
      a <- lit(1)
      b <- lit(2)
      x <- lt(a, b)
      c <- lit(3)
      d <- lit(4)
      y <- lt(c, d)
      z <- and(x, y)
    } yield z
  }
}

object Interpreter {
  import cats.Id

  implicit val idHandler: ExprAlg.Handler[Id] =
    new ExprAlg.Handler[Id] {
      override def lit[A](a: A): Id[A] =
        a

      override def lt(a: Int, b: Int): Id[Boolean] =
        a < b

      override def and(a: Boolean, b: Boolean): Id[Boolean] =
        a && b
    }

  implicit val eitherHandler: ExprAlg.Handler[Either[String, ?]] =
    new ExprAlg.Handler[Either[String, ?]] {
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
