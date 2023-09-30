// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object CW8a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList


// (1) Implement below the shunting yard algorithm. The most
// convenient way to this in Scala is to implement a recursive 
// function and to heavily use pattern matching. The function syard 
// takes some input tokens as first argument. The second and third 
// arguments represent the stack and the output of the shunting yard 
// algorithm.
//
// In the marking, you can assume the function is called only with 
// an empty stack and an empty output list. You can also assume the
// input os  only properly formatted (infix) arithmetic expressions
// (all parentheses will be well-nested, the input only contains 
// operators and numbers).

// You can implement any additional helper function you need. I found 
// it helpful to implement two auxiliary functions for the pattern matching:  
// 

def is_op(op: String) : Boolean = {
	if (ops.contains(op)) true
	else false
}
def prec(op1: String, op2: String) : Boolean = {
	if (precs(op2) > precs(op1)) true
	else if (precs(op2) == precs(op1)) true
	else false
}

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
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

 
// (2) Implement a compute function that evaluates an input list
// in postfix notation. This function takes a list of tokens
// and a stack as argumenta. The function should produce the 
// result as an integer using the stack. You can assume 
// this function will be only called with proper postfix 
// expressions.    

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
		case Nil => st(0)
	}
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}


