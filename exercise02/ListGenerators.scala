object ListGenerators {

    def myMap: (Int => Int) => List[Int] => List[Int] = f => l =>
        for (e <- l) yield f(e)

    def flatMap: (Int => List[Int]) => List[Int] => List[Int] = f => l =>
        for {e <- l
             r <- f(e)} yield r

    def pairSums: (Int, Int) => List[(Int, Int)] = (n, s) =>
        for {i <- List.range(0, n + 1)
             j <- List.range(0, n + 1) if i + j == s} yield (i, j)
}