import scala.io.Source
import scala.collection.mutable.Queue

def solve(input: Array[String], plus: Int, times: Int): Long =
  input
    .map(line =>
      compute_expr(tokenize(line), 1, Map('+' -> plus, '*' -> times))
    )
    .sum

def partOne(input: Array[String]): Long = solve(input, 1, 1)
def partTwo(input: Array[String]): Long = solve(input, 2, 1)

object Main {
  def main(args: Array[String]) = {
    val lines = Source.fromFile("input").getLines.toArray
    println(partOne(lines))
    println(partTwo(lines))
  }
}

enum Token:
  case Number(num: Long)
  case Operator(op: Char)
  case LeftParam
  case RightParam

def tokenize(line: String): Queue[Token] = {
  import java.io.StreamTokenizer

  val LEFT_PARAM = '('
  val RIGHT_PARAM = ')'

  var reader = new java.io.StringReader(line)
  var tokens = new StreamTokenizer(reader)
  var q: Queue[Token] = Queue()

  var curr = tokens.nextToken()

  while curr != StreamTokenizer.TT_EOF do
    if tokens.ttype == StreamTokenizer.TT_NUMBER then
      q.enqueue(Token.Number(tokens.nval.toLong))
    else if tokens.ttype == LEFT_PARAM then q.enqueue(Token.LeftParam)
    else if tokens.ttype == RIGHT_PARAM then q.enqueue(Token.RightParam)
    else q.enqueue(Token.Operator(tokens.ttype.toChar))

    curr = tokens.nextToken()

  return q
}

def compute_atom(q: Queue[Token], opInfo: Map[Char, Int]): Long = {
  val tok = q.front;

  tok match
    case Token.LeftParam =>
      q.dequeue()
      val v = compute_expr(q, 1, opInfo)
      assert(q.front == Token.RightParam)
      q.dequeue()
      return v
    case Token.Number(num) =>
      q.dequeue()
      return num
    case _ => throw new Exception("Error")
}

def compute_expr(
    q: Queue[Token],
    min_prec: Int,
    opInfo: Map[Char, Int]
): Long = {
  var atom_lhs = compute_atom(q, opInfo)

  var running = true
  while running do

    if q.isEmpty then return atom_lhs

    q.front match
      case Token.Operator(op) =>
        if opInfo(op) < min_prec then return atom_lhs

        val prec = opInfo(op)
        val next_min_prev = prec + 1
        q.dequeue()

        val atom_rhs = compute_expr(q, next_min_prev, opInfo)
        atom_lhs = compute_op(op, atom_lhs, atom_rhs)

      case _ => running = false

  return atom_lhs
}

def compute_op(op: Char, lhs: Long, rhs: Long): Long = {
  if op == '+' then return lhs + rhs
  else if op == '*' then return lhs * rhs
  else throw new Exception("Unknown operation")
}
