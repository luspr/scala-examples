object Recursion {

    def myLength: List[Any] => Int = {
        case _::ts  => 1 + myLength(ts)
        case Nil    => 0
    }

    def myConcat: (List[Any], List[Any]) => List[Any] = {
        case (Nil, ys)   => ys
        case (x::xs, ys) => x::myConcat(xs, ys)
    }

    def myAppend: (List[Any], Any) => List[Any] = (l, e) => myConcat(l, List(e))

    // alternative
    def myAppend2: (List[Any], Any) => List[Any] = {
        case (Nil, e)   => List(e)
        case (h::ts, e) => h::myAppend(ts, e)
    }
    
    def mySum: List[Int] => Int = {
        case x::xs => x + mySum(xs)
        case Nil   => 0
    }

    def myProduct: List[Int] => Int = {
        case x::xs => x * myProduct(xs)
        case Nil   => 1
    }
}
