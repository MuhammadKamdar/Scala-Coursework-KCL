// Preliminary Part about the 3n+1 conjecture
//============================================

object CW6a {

//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.
//testing

def collatz(n: Long) : Long = {
    if (n%2 == 0){
        val count = collatz(n/2) + 1
        count
    } 
    else if (n%2 != 0 && n != 1){
        val count = collatz((n*3)+1) + 1
        count
    }
    else {
        val count = 0
        count
    }
}


//(2) Complete the collatz_max function below. It should
//    calculate how many steps are needed for each number 
//    from 1 up to a bound and then calculate the maximum number of
//    steps and the corresponding number that needs that many 
//    steps. Again, you should expect bounds in the range of 1
//    up to 1 Million. The first component of the pair is
//    the maximum number of steps and the second is the 
//    corresponding number.

def collatz_max(bnd: Long) : (Long, Long) = {
    val list = (1 to bnd.toInt).toList
    val collatzList = list.map(n => collatz(n))
    val a = collatzList.max
    val b = collatzList.indexOf(a)+1
    val ans = (a,b.toLong)
    ans
}

//(3) Implement a function that calculates the last_odd
//    number in a collatz series.  For this implement an
//    is_pow_of_two function which tests whether a number 
//    is a power of two. The function is_hard calculates 
//    whether 3n + 1 is a power of two. Again you can
//    assume the input ranges between 1 and 1 Million,
//    and also assume that the input of last_odd will not 
//    be a power of 2.

def is_pow_of_two(n: Long) : Boolean = {
    val q = n&(n-1)
    if (q == 0) true
    else false
}

def is_hard(n: Long) : Boolean = {
    if (is_pow_of_two((3*n)+1)) true
    else false
}
def collatz_calculate(n: Long) : Long = {
    if (n%2 == 0){
        val ans = n/2
        if (ans%2 == 0){
            collatz_calculate(ans)
        }
        else ans
    }
    else {
        val ans = (3*n) + 1
        if (ans%2 == 0){
            collatz_calculate(ans)
        }
        else ans
    }
}

def last_odd(n: Long) : Long = {
    val b = collatz_calculate(n)
    if (is_hard(b)){
        b
    }
    else{
        last_odd(b)
    }
}

}

