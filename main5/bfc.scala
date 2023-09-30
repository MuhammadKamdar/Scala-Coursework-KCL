// Core Part about a "Compiler" for the Brainf*** language
//======================================================


object CW10b {


// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._
import java.io.FileNotFoundException
import scala.annotation.tailrec

def load_bff(name: String) : String = {
    try {
        val bff = io.Source.fromFile(name).getLines().mkString(" ")
        bff
    }
    catch {
        case ex: FileNotFoundException =>
        val bff = ""
        bff
    }
}


def sread(mem: Mem, mp: Int) : Int = {
    mem.getOrElse(mp,0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
    val newMem = mem + (mp -> v)
    newMem
}


// TASKS
//=======

// (5) Write a function jtable that precomputes the "jump
//     table" for a bf-program. This function takes a bf-program 
//     as an argument and Returns a Map[Int, Int]. The 
//     purpose of this map is to record the information about
//     pc positions where '[' or a ']' are stored. The information
//     is to which pc-position do we need to jump next?
// 
//     For example for the program
//    
//       "+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"
//
//     we obtain the map
//
//       Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)
//  
//     This states that for the '[' on position 5, we need to
//     jump to position 20, which is just after the corresponding ']'.
//     Similarly, for the ']' on position 19, we need to jump to
//     position 6, which is just after the '[' on position 5, and so
//     on. The idea is to not calculate this information each time
//     we hit a bracket, but just look up this information in the 
//     jtable. You can use the jumpLeft and jumpRight functions
//     from Part 1 for calculating the jtable.
//
//     Then adapt the compute and run functions from Part 1 
//     in order to take advantage of the information stored in the jtable. 
//     This means whenever jumpLeft and jumpRight was called previously,
//     you should immediately look up the jump address in the jtable.
 
@tailrec
def jumpRight(prog: String, pc: Int, level: Int) : Int = {
    if (pc > prog.length-1) pc
    else {
        prog(pc) match{
            case '[' => jumpRight(prog, pc+1, level+1)
            case ']' => if (level == 0) pc+1
                        else jumpRight(prog, pc+1, level-1)
            case _ => jumpRight(prog, pc+1, level)
        }
    }
}

def jtable(pg: String) : Map[Int, Int] = {
  val a = for (x<- (0 to pg.length-1).toList)yield{
    if (pg(x) == '['){
      val a = jumpRight(pg, x+1, 0)
      val b = List((a-1,x+1),(x,a))
      b
    }
    else None
  }
  a.flatten.toMap
}




// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)

@tailrec
def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc > pg.length-1) mem
    else {
        pg(pc) match{
            case '>' => compute2(pg,tb, pc+1, mp+1, mem)
            case '<' => compute2(pg,tb, pc+1, mp-1, mem)
            case '+' => compute2(pg,tb, pc+1, mp,write(mem,mp,sread(mem,mp)+1))
            case '-' => compute2(pg,tb, pc+1, mp,write(mem,mp,sread(mem,mp)-1))
            case '.' => print(sread(mem,mp).toChar)
                        compute2(pg, tb,pc+1, mp, mem)
            case '[' => if (sread(mem,mp) == 0) compute2(pg,tb,tb.get(pc).get,mp,mem)
                        else compute2(pg,tb,pc+1,mp,mem)
            case ']' => if (sread(mem,mp) != 0) compute2(pg,tb,tb.get(pc).get,mp,mem)
                        else compute2(pg,tb,pc+1,mp,mem)
            case '*' => compute2(pg, tb,pc+1,mp, write(mem,mp,sread(mem,mp)*sread(mem,mp-1)))
            case '@' => compute2(pg,tb,pc+1,mp, write(mem,sread(mem,mp),sread(mem,mp-1)))
            case '#' => print(sread(mem,mp))
                        compute2(pg,tb,pc+1, mp, mem)
            case _ => compute2(pg,tb,pc+1,mp,mem)
        }
    }
}
def run2(pg: String, m: Mem = Map()) = {
  val jumpMap = jtable(pg)
  compute2(pg, jumpMap,0,0, m)
}


// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (6) Write a function optimise which deletes "dead code" (everything
// that is not a bf-command) and also replaces substrings of the form
// [-] by a new command 0. The idea is that the loop [-] just resets the
// memory at the current location to 0. In the compute3 and run3 functions
// below you implement this command by writing the number 0 to mem(mp), 
// that is write(mem, mp, 0). 
//
// The easiest way to modify a string in this way is to use the regular
// expression """[^<>+-.,\[\]]""", which recognises everything that is 
// not a bf-command and replace it by the empty string. Similarly the
// regular expression """\[-\]""" finds all occurrences of [-] and 
// by using the Scala method .replaceAll you can replace it with the 
// string "0" standing for the new bf-command.

def optimise(s: String) : String = {
  val reg = """[^<>+-.,\[\]@#*]""".r
  val reg2 = """\[-\]""".r
  val a = reg.replaceAllIn(s,"")
  reg2.replaceAllIn(a,"0")
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc > pg.length-1) mem
    else {
        pg(pc) match{
            case '>' => compute3(pg,tb, pc+1, mp+1, mem)
            case '<' => compute3(pg,tb, pc+1, mp-1, mem)
            case '+' => compute3(pg,tb, pc+1, mp,write(mem,mp,sread(mem,mp)+1))
            case '-' => compute3(pg,tb, pc+1, mp,write(mem,mp,sread(mem,mp)-1))
            case '.' => print(sread(mem,mp).toChar)
                        compute3(pg, tb,pc+1, mp, mem)
            case '[' => if (sread(mem,mp) == 0) compute3(pg,tb,tb.get(pc).get,mp,mem)
                        else compute3(pg,tb,pc+1,mp,mem)
            case ']' => if (sread(mem,mp) != 0) compute3(pg,tb,tb.get(pc).get,mp,mem)
                        else compute3(pg,tb,pc+1,mp,mem)
            case '0' => compute3(pg,tb,pc+1,mp,write(mem,mp,0))
            case '*' => compute3(pg, tb,pc+1,mp, write(mem,mp,sread(mem,mp)*sread(mem,mp-1)))
            case '@' => compute3(pg,tb,pc+1,mp, write(mem,sread(mem,mp),sread(mem,mp-1)))
            case '#' => print(sread(mem,mp))
                        compute3(pg,tb,pc+1, mp, mem)
            case _ => compute3(pg,tb,pc+1,mp,mem)
        }
    }
}

def run3(pg: String, m: Mem = Map()) = {
  val op = optimise(pg)
  val jumpMap = jtable(op)
  compute3(op, jumpMap, 0, 0, m)
}


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (7)  Write a function combine which replaces sequences
// of repeated increment and decrement commands by appropriate
// two-character commandss. For example for sequences of +
//
//              orig bf-cmds  | replacement
//            ------------------------------
//              +             | +A 
//              ++            | +B
//              +++           | +C
//                            |
//              ...           |
//                            | 
//              +++....+++    | +Z
//                (where length = 26)
//
//  Similar for the bf-command -, > and <. All other commands should
//  be unaffected by this change.
//
//  Adapt the compute4 and run4 functions such that they can deal
//  appropriately with such two-character commands.

def count(s: String) : String = {
  val size = s.length
  val rem = (size%26)
  val whole = size/26
  if (whole > 0 && rem == 0){
    (s(0).toString + "Z")*whole
  }
  else if (whole > 0 && rem > 0){
    (s(0).toString + "Z")*whole + s(0).toString + (rem+64).toChar.toString
  }
  else s(0).toString + (rem+64).toChar.toString
}

def combine(s: String) : String = {
  val bffCom = List('+', '-', '<', '>')
  val a = s.split("(?<=(.))(?!\\1)").toList
  val countList = for (x <- a)yield{
    if (bffCom.contains(x(0))){
      count(x)
    } else x
  }
  countList.mkString
}



// testcase
// combine(load_bff("benchmark.bf"))


def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc > pg.length-1) mem
    else {
        pg(pc) match{
            case '>' => val n = pg(pc+1).toInt - 64
                        compute4(pg,tb, pc+2, mp+n, mem)
            case '<' => val n = pg(pc+1).toInt - 64
                        compute4(pg,tb, pc+2, mp-n, mem)
            case '+' => val n = pg(pc+1).toInt - 64
                        compute4(pg,tb, pc+2, mp,write(mem,mp,sread(mem,mp)+n))
            case '-' => val n = pg(pc+1).toInt - 64
                        compute4(pg,tb, pc+2, mp,write(mem,mp,sread(mem,mp)-n))
            case '.' => print(sread(mem,mp).toChar)
                        compute4(pg, tb,pc+1, mp, mem)
            case '[' => if (sread(mem,mp) == 0) compute4(pg,tb,tb.get(pc).get,mp,mem)
                        else compute4(pg,tb,pc+1,mp,mem)
            case ']' => if (sread(mem,mp) != 0) compute4(pg,tb,tb.get(pc).get,mp,mem)
                        else compute4(pg,tb,pc+1,mp,mem)
            case '0' => compute4(pg,tb,pc+1,mp,write(mem,mp,0))
            case '*' => compute4(pg, tb,pc+1,mp, write(mem,mp,sread(mem,mp)*sread(mem,mp-1)))
            case '@' => compute4(pg,tb,pc+1,mp, write(mem,sread(mem,mp),sread(mem,mp-1)))
            case '#' => print(sread(mem,mp))
                        compute4(pg,tb,pc+1, mp, mem)
            case _ => compute4(pg,tb,pc+1,mp,mem)
        }
    }
}

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = {
  val comb = combine(optimise(pg))
  val jumpMap = jtable(comb)
  compute4(comb, jumpMap, 0, 0, m)
}


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}
