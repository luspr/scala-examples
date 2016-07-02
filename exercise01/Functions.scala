object Functions {

    def f1: Int => Int = x => -x

    def f2: Int => Int = x => {
         if(x >= 0) x + 10 else x - 2
    }

    def f3: (Any, Any) => (Any, Any) = (x, y) => (y, x)

    def f4: ((Any, Any)) => (Any, Any) = tuple => (tuple._2, tuple._1)

    def square: Double => Double = x => x * x

    def abs: Int => Int = n => n match {
        case m if (m < 0) => -m
        case m => m
    }

    def fac: Int => Int = n => if (n == 0) 1 else n * fac(n - 1)

    def fac2: Int => Int = n => List.range(1, n + 1).product
    
    // tail-recursive factorial implementation
    def facTail: Int => Int = n => {
        def facHelp: (Int, Int) => Int = (i, k) => if (i == 0) k else facHelp(i - 1, i * k)
        facHelp(n, 1)
    }

    def quadraticEquation: (Double, Double, Double) => List[Double] = (a, b, c) => {
        val x_1 = (-b + math.sqrt(square(b) - 4 * a * c)) / (2 * a)
        val x_2 = (-b - math.sqrt(square(b) - 4 * a * c)) / (2 * a)
        List(x_1, x_2)
    }

}
