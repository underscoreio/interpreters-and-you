package direct

object Program {
  val program: () => Boolean =
    () => 1 < 2 && 3 < 4
}

object Interpreter {
  def eval[A](program: () => A): A =
    program()
}

object Main extends App {
  import Program._

  println("Program: " + program)

  import Interpreter._

  println("Result: " + eval(program))
}
