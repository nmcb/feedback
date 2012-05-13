package feedback.playground

import Calculator._;

object Client {

  val expressions = List(
    "1",
    "(2)",
    "3 + 0",
    "3 + 2",
    "(0 + 6)",
    "(7 + 8) + 9",
    "(1 + 2) + (3 + 4)",
    "(1 * 6) / (7 * 1)",
    "9 - 1",
    "(2 - 3) - 4",
    "(5 / 6) / 7",
    "(2 / 2) / (2 / 2)"
  )

  def parsing() {
    Console.println("\nPARSING")
    for (text <- expressions)
      Console.printf("%20s  =>  %s\n", text, parse(text))
  }

  def simplifying() {
    Console.println("\nSIMPLIFYING")
    for (text <- expressions)
      Console.printf("%20s  =>  %s\n", text, simplify(parse(text)))
  }

  def evaluating() {
    Console.println("\nEVALUATING")
    for (text <- expressions)
      Console.printf("%20s  ==  %s\n", text, evaluate(simplify(parse(text))))
  }

  def main(args: Array[String]) {
    parsing()
    simplifying()
    evaluating()
  }
}
