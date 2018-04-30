object Program {
  val program = () =>
    1 * 2 + 3 * 4
}

object Interpreter {
  def eval(program: () => Int): Int =
    program()
}

object Main extends App {
  import Program.program
  import Interpreter.eval

  println("Program: " + program)
  println("Result: " + eval(program))
}
