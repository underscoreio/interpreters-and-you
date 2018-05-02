package direct

object Program {
  val program: () => Int =
    () => 1 * 2 + 3 * 4
}

object Interpreter {
  def eval(program: () => Int): Int =
    program()
}

object Main extends App {
  import Program._

  println("Program: " + program)

  import Interpreter._

  println("Result: " + eval(program))
}
