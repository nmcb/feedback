package feedback.playground

import util.parsing.combinator.JavaTokenParsers

abstract class Expr

case class Val(value: Double) extends Expr

case class UnOp(operator: String, operand: Expr) extends Expr

case class BiOp(operator: String, lhs: Expr, rhs: Expr) extends Expr

object Calculator {

   def parse(s: String): Expr = {

      object ExpressionParser extends JavaTokenParsers {
         def expr: Parser[Expr] =
            (term ~ "+" ~ term) ^^ { case lhs ~ plus ~ rhs => BiOp("+", lhs, rhs) } |
            (term ~ "-" ~ term) ^^ { case lhs ~ minus ~ rhs => BiOp("-", lhs, rhs) } |
            term

         def term: Parser[Expr] =
            (factor ~ "*" ~ factor) ^^ { case lhs ~ times ~ rhs => BiOp("*", lhs, rhs) } |
            (factor ~ "/" ~ factor) ^^ { case lhs ~ div ~ rhs => BiOp("/", lhs, rhs) } |
            factor

         def factor: Parser[Expr] =
            "(" ~> expr <~ ")" | floatingPointNumber ^^ { x => Val(x.toDouble) }

         def parse(s: String) = parseAll(expr, s)
      }

      ExpressionParser.parse(s).get
   }

   def simplify(e: Expr): Expr = {

      // simplifies root-expression (business rules)
      def combine(e: Expr) = e match {
         case UnOp("-", UnOp("-", x)) => x
         case UnOp("+", x) => x
         case BiOp("*", x, Val(1)) => x
         case BiOp("*", Val(1), x) => x
         case BiOp("*", x, Val(0)) => Val(0)
         case BiOp("*", Val(0), x) => Val(0)
         case BiOp("/", x, Val(1)) => x
         case BiOp("/", x1, x2) if x1 == x2 => Val(1)
         case BiOp("+", x, Val(0)) => x
         case BiOp("+", Val(0), x) => x
         case _ => e
      }

      // simplify the sub-expressions (AST combinator expressions)
      val subs = e match {
         case BiOp(op, lhs, rhs) => BiOp(op, simplify(lhs), simplify(rhs))
         case UnOp(op, operand) => UnOp(op, simplify(operand))
         case _ => e
      }

      combine(subs)
   }

   def evaluate(e: Expr): Double = {
      e match {
         case Val(x) => x
         case UnOp("-", x) => -evaluate(x)
         case BiOp("+", l, r) => (evaluate(l) + evaluate(r))
         case BiOp("-", l, r) => (evaluate(l) - evaluate(r))
         case BiOp("*", l, r) => (evaluate(l) * evaluate(r))
         case BiOp("/", l, r) => (evaluate(l) / evaluate(r))
      }
   }
}
