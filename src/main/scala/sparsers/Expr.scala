package sparsers

sealed trait Expr {
  def eval: Int
}

case class Add(lhs: Expr, rhs: Expr) extends Expr {
  def eval = lhs.eval + rhs.eval
}

case class Sub(lhs: Expr, rhs: Expr) extends Expr {
  def eval = lhs.eval - rhs.eval
}

case class Mult(lhs: Expr, rhs: Expr) extends Expr {
  def eval = lhs.eval * rhs.eval
}

case class Div(lhs: Expr, rhs: Expr) extends Expr {
  def eval = lhs.eval / rhs.eval
}

case class Value(eval: Int) extends Expr
