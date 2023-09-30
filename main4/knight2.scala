// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object CW9b {


// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

val knightMoves = List((1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2))

def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  if ((!path.contains(x)) && (x._1<dim && x._1> -1) && (x._2<dim && x._2> -1)) true
  else false
}


def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val moves = knightMoves.map(n=>(n._1+x._1,n._2+x._2))
  val ans = moves.filter(is_legal(dim, path, _))
  ans
}

def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  xs match{
    case Nil => None
    case x::xs => val result = f(x)
    if (result != None) result
    else first(xs,f)
  }
}

def first_tour(dim: Int, path: Path) : Option[Path] = {
  if (path.length >= dim*dim)  Some(path)
  else {
    val moves = legal_moves(dim, path, path(0))
    first(moves, (x:Pos) => first_tour(dim, x::path))
  }
}

//(6) Complete the function that calculates a list of onward
//    moves like in (2) but orders them according to Warnsdorf’s 
//    rule. That means moves with the fewest legal onward moves 
//    should come first.


def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val a = legal_moves(dim, path, x)
    val b = for (z <- a)yield{
        (z,legal_moves(dim, z::path, z).size)
    }
    b.sortBy(_._2).map(x => x._1)
}


//(7) Complete the function that searches for a single *closed* 
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board. s


def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if (path.length > dim*dim) {
        val origin :: rest = path
        Some(rest)
    }
    else {
        if (path.length == dim*dim){
            val origin :: rest = path.reverse
            val moves = ordered_moves(dim, rest, path(0))
            first(moves, (x:Pos) => first_closed_tour_heuristics(dim, x::path))
        }
        else {
            val moves = ordered_moves(dim, path, path(0))
            first(moves, (x:Pos) => first_closed_tour_heuristics(dim, x::path))
        }
        
    }
}

//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
     if (path.length >= dim*dim)  Some(path)
  else {
    val moves = ordered_moves(dim, path, path(0))
    first(moves, (x:Pos) => first_tour_heuristics(dim, x::path))
  }
}
//time_needed(first_tour_heuristics(8,List((0,0))))
}
