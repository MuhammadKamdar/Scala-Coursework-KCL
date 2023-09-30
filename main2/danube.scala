// Core Part about Movie Recommendations 
// at Danube.co.uk
//===========================================

object CW7b {

import io.Source
import scala.util._
import java.io.FileNotFoundException
import scala.annotation.tailrec

// (1) Implement the function get_csv_url which takes an url-string
//     as argument and requests the corresponding file. The two urls
//     of interest are ratings_url and movies_url, which correspond 
//     to CSV-files.
//
//     The function should ReTurn the CSV-file appropriately broken
//     up into lines, and the first line should be dropped (that is without
//     the header of the CSV-file). The result is a list of strings (lines
//     in the file).dd

def get_csv_url(url: String) : List[String] = {
    try {
        val ansList = io.Source.fromURL(url,"ISO-8859-1").getLines().drop(1).toList
        ansList
    }
    catch {
        case ex: FileNotFoundException =>
        val ansList = List()
        ansList
    }
}
// val movies = get_csv_url(movies_url)
// val ratings = get_csv_url(ratings_url)

val ratings_url = """https://nms.kcl.ac.uk/christian.urban/ratings.csv"""
val movies_url = """https://nms.kcl.ac.uk/christian.urban/movies.csv"""

// testcases
//-----------
//:
//C

//ratings.length  // 87313
//movies.length   // 9742



// (2) Implement two functions that process the CSV-files from (1). The ratings
//     function filters out all ratings below 4 and ReTurns a list of 
//     (userID, movieID) pairs. The movies function just ReTurns a list 
//     of (movieID, title) pairs. Note the input to these functions, that is
//     the argument lines, will be the output of the function get_csv_url.


def process_ratings(lines: List[String]) : List[(String, String)] = {
    for (x <- lines;
        if (x.split(",")(2) > "3")) yield (x.split(",")(0), x.split(",")(1))
}

def process_movies(lines: List[String]) : List[(String, String)] = {
    val moviePairs = for (x <- lines) yield {
        val movie = x.split(",")
        val pair = (movie(0),movie(1))
        pair
    }
    moviePairs
}


// testcases
//-----------
// val good_ratings = process_ratings(ratings)
// val movie_names = process_movies(movies)

// good_ratings.length   //48580
// movie_names.length    // 9742


// (3) Implement a grouping function that calculates a Map
//     containing the userIDs and all the corresponding recommendations 
//     (list of movieIDs). This  should be implemented in a tail
//     recursive fashion, using a Map m as accumulator. This Map m
//     is set to Map() at the beginning of the calculation.
// @tailrec

def groupById(ratings: List[(String, String)], m: Map[String, List[String]]) : Map[String, List[String]] = {
    ratings match{
        case Nil => m
        case x :: rest if m.contains(x._1)
        => val list : List[String] = m.get(x._1).get ++ List(x._2)
        val new_m = m + (x._1 -> list)
        groupById(rest, new_m)
        case x :: rest if !m.contains(x._1)
        => val list : List[String] = List() :+ x._2
        val new_m = m + (x._1 -> list)
        groupById(rest, new_m)
    }
}

// testcases
//-----------
// val ratings_map = groupById(good_ratings, Map())
// val movies_map = movie_names.toMap

//ratings_map.get("414").get.map(movies_map.get(_)) 
//    => most prolific recommender with 1227 positive ratings

//ratings_map.get("474").get.map(movies_map.get(_)) 
//    => second-most prolific recommender with 787 positive ratings

//ratings_map.get("214").get.map(movies_map.get(_)) 
//    => least prolific recommender with only 1 positive rating



// (4) Implement a function that takes a ratings map and a movie_name as argument.
//     The function calculates all suggestions containing
//     the movie in its recommendations. It ReTurns a list of all these
//     recommendations (each of them is a list and needs to have the movie deleted, 
//     otherwise it might happen we recommend the same movie).


def favourites(m: Map[String, List[String]], mov: String) : List[List[String]] = {
    val mapKeys = m.keys.toList
    for (x <- mapKeys;
        if (m.get(x).get.contains(mov)))yield (m.get(x).get.filter(_ != mov))
}


// testcases
//-----------
// movie ID "912" -> Casablanca (1942)
//          "858" -> Godfather
//          "260" -> Star Wars: Episode IV - A New Hope (1977)

//favourites(ratings_map, "912").length  // => 80

// That means there are 80 users that recommend the movie with ID 912.
// Of these 80  users, 55 gave a good rating to movie 858 and
// 52 a good rating to movies 260, 318, 593.



// (5) Implement a suggestions function which takes a ratings
//     map and a movie_name as arguments. It calculates all the recommended
//     movies sorted according to the most frequently suggested movie(s) first.

def suggestions(recs: Map[String, List[String]], mov_name: String) : List[String] = {
    val favouriteList = favourites(recs, mov_name).flatten
    val set = favouriteList.distinct
    val countMap = favouriteList.groupBy(identity).view.mapValues(_.size).toMap
    val pairs = set.map(n=>(n,countMap.get(n).get))
    pairs.sortBy(_._2).reverse.map(n=>n._1)
}
// testcases
//-----------

//suggestions(ratings_map, "912")
//suggestions(ratings_map, "912").length  
// => 4110 suggestions with List(858, 260, 318, 593, ...)
//    being the most frequently suggested movies


// (6) Implement a recommendations function which generates at most
//     *two* of the most frequently suggested movies. It ReTurns the 
//     actual movie names, not the movieIDs.


def recommendations(recs: Map[String, List[String]], movs: Map[String, String], mov_name: String) : List[String] = {
    val suggested = suggestions(recs, mov_name).take(2)
    suggested.map(n => movs.get(n).get)
    }

// testcases
//-----------
// recommendations(ratings_map, movies_map, "912")
//   => List(Godfather, Star Wars: Episode IV - A NewHope (1977))

//recommendations(ratings_map, movies_map, "260")
//   => List(Star Wars: Episode V - The Empire Strikes Back (1980), 
//           Star Wars: Episode VI - Return of the Jedi (1983))

// recommendations(ratings_map, movies_map, "2")
//   => List(Lion King, Jurassic Park (1993))

// recommendations(ratings_map, movies_map, "0")
//   => Nil

// recommendations(ratings_map, movies_map, "1")
//   => List(Shawshank Redemption, Forrest Gump (1994))

// recommendations(ratings_map, movies_map, "4")
//   => Nil  (there are three ratings for this movie in ratings.csv but they are not positive)     



// (7) Calculate the recommendations for all movies according to
// what the recommendations function in (6) produces (this
// can take a few seconds). Put all recommendations into a list 
// (of strings) and count how often the strings occur in
// this list. This produces a list of string-int pairs,
// where the first component is the movie name and the second
// is the number of how many times the movie was recommended. 
// Sort all the pairs according to the number
// of times they were recommended (most recommended movie name 
// first).

def most_recommended(recs: Map[String, List[String]],
                     movs: Map[String, String]) : List[(String, Int)] = {
                     val movieList = movs.keys.toList
                     val recomList = for (x <- movieList)yield{
                         recommendations(recs, movs, x)
                    }
                    val all = recomList.flatten
                    val set = all.distinct
                    val pairs = set.map(y => (y, all.count(_==y)))
                    pairs.sortBy(_._2).reverse
                    }

// testcase
//
//most_recommended(ratings_map, movies_map).take(3)
// =>
// List((Matrix,698), 
//      (Star Wars: Episode IV - A New Hope (1977),402), 
//      (Jerry Maguire (1996),382))



}
