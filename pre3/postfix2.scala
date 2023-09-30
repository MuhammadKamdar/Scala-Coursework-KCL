// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object CW8b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

def is_op(op: String) : Boolean = {
	if (ops.contains(op)) true
	else false
}
def prec(op1: String, op2: String) : Boolean = {
	if (precs(op2) > precs(op1)) true
	else if ((precs(op2) == precs(op1)) && (assoc(op2) != RA)) true
	else false
}

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def removeFirst(toks: Toks) : Toks = {
	toks match {
		case x :: rest => rest
		case Nil => Nil
	}
}

def removeFirstTwoList(listInt: List[Int]) : List[Int] = {
	listInt match {
		case _ :: _ :: rest => rest
		case _ :: rest => rest
		case Nil => Nil
	}
}


def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
	toks match {
		// for digits
		case x :: rest if x.forall(_.isDigit)
			=> val b = out ++ split(x)
			syard(rest, st,b)
		// for left-bracket
		case "(" :: rest => val stack = split("(") ++ st
			syard(rest, stack, out)
		// for operators
		case x :: rest if is_op(x)
			=> if ((st.isEmpty == false) && (is_op(st(0))) && (prec(x, st(0)))){
				val b = out ++ split(st(0))
				val staResest = removeFirst(st)
				val stack = split(x) ++ staResest
				syard(rest, stack, b)
			}
			else {
				val stack = split(x) ++ st
				syard(rest, stack, out)
			}
		// for right bracket
		case ")" :: rest => 
			val index = (0 to st.indexOf("(")-1).toList
			val index2 = (st.indexOf("(")+1 to st.size-1).toList
			val newSt = for (x <- index2) yield {
				st(x)
			}
			val addOut = for (x <- index) yield {
				st(x)
			}
			val newOut = out ++ addOut
			syard(rest, newSt, newOut)
		// for empty list
		case Nil => val ans = out ++ st
		ans
	}
}

// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.
def compute(toks: Toks, st: List[Int] = Nil) : Int = {
	toks match {
		case x :: rest if x.forall(_.isDigit)
			=> val stack = List(x.toInt) ++ st
			compute(rest, stack)
		case "+" :: rest 
			=> val b = st(0) + st(1)
			val temp = removeFirstTwoList(st)
			val stack = List(b.toInt) ++ temp
			compute(rest, stack)
		case "-" :: rest 
			=> val b = st(1) - st(0)
			val temp = removeFirstTwoList(st)
			val stack = List(b.toInt) ++ temp
			compute(rest, stack)
		case "*" :: rest 
			=> val b = st(0) * st(1)
			val temp = removeFirstTwoList(st)
			val stack = List(b.toInt) ++ temp
			compute(rest, stack)
		case "/" :: rest 
			=> val b = st(1) / st(0)
			val temp = removeFirstTwoList(st)
			val stack = List(b.toInt) ++ temp
			compute(rest, stack)
    case "^" :: rest
      => val b =  BigInt(st(1)).pow(st(0)).toInt
      val temp = removeFirstTwoList(st)
			val stack = List(b.toInt) ++ temp
			compute(rest, stack)
		case Nil => st(0)
	}
}



// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
