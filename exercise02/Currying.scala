object Currying {

    def curry: ((Int, Int ) => Int) => (Int => Int => Int) = f => a => b => f(a, b)

    def uncurry: (Int => Int => Int) => ((Int, Int) => Int) = f => (a, b) => f(a)(b)

    def isAscending: (Int, Int) => Boolean = (n, m) => n <= m

    def isDescending: (Int, Int) => Boolean = (n, m) => n > m

    def curriedSort: ((Int, Int) => Boolean) => List[Int] => List[Int] =
                                f => list => list.sortWith(f)

    def descendingSort = curriedSort(isDescending)

    // also possible: definition as val
    val ascendingSort = curriedSort(isAscending)

    // == test functions ==
    def add(x: Int, y: Int): Int = x + y // add(12, 12)

    def add2: Int => Int => Int = x => y => x + y // add(12)(12)

    def div: Int => Int => Int = y => x => x / y

    // this function adds ten to a given integer
    val addTen = add2(10) // addTen(32) == 42

}