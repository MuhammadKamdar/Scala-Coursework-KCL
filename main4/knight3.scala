// Finding a single tour on a "mega" board
//=========================================

object CW9c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.
import scala.annotation.tailrec

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

def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val a = legal_moves(dim, path, x)
    val b = for (z <- a)yield{
        (z,legal_moves(dim, z::path, z).size)
    }
    b.sortBy(_._2).map(x => x._1)
}

//(9) Implement a function that searches for a 
//    you have to be careful to write a tail-recursive version as this 
//    function will be called with dimensions of up to 70 * 70
//    and starting field (0, 0). It has to produce a solution within
//    30 secondss.

@tailrec
def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
    if (path.length >= dim*dim)  Some(path)
    else {
        val moves = ordered_moves(dim, path, path(0))
        tour_on_mega_board(dim, moves.head::path)
    }
}
}
