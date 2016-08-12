object Lazy {

    val natural = {
        def naturalHelper: Long => Stream[Long] = n => n #:: naturalHelper(n + 1)
        naturalHelper(1)
    }

    def limit: Stream[Long] => Long => List[Long] =  s => l =>
        s.takeWhile(_ <= l).toList

    def interval: Stream[Long] => (Long, Long) => List[Long] = s => (k, l) =>
        s.takeWhile(_ <= l).toList.filter(_ >= k)
        // limit(s.dropWhile(_ >= k))

    def factorial: Int => Long = k => {
        def factorialHelper: (Int, Long) => Stream[Long] = (i, acc) => {
      //      println(i, acc)
            acc #:: factorialHelper(i + 1, acc * i)
        }

        factorialHelper(1, 1)(k)
    }

    def euler: Int => Double = n => {
        def eulerHelper: Int => Stream[Double] = k => {
            (1.0 / factorial(k)) #:: eulerHelper(k + 1)
        }
        eulerHelper(0).take(n).sum
    }
}