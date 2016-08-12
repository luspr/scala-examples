object Fold {

    def foldRight: List[Int] => ((Int, List[Int]) => List[Int]) => List[Int]
           => List[Int] = ys => f => xs => xs match {
        case Nil        => ys
        case head::tail => f(head, foldRight(ys)(f)(tail))
   }

   def foldLeft: List[Int] => ((List[Int], Int) => List[Int]) => List[Int]
           => List[Int] = ys => f => xs => {
        println("ys " + ys + " xs: " + xs);
        xs match {
            case Nil        => ys
            case head::tail => foldLeft(f(ys, head))(f)(tail)
        }
    }















    def main(args: Array[String]): Unit = {

        // println(List.range(1,8).foldLeft("0")((c,d) => "("+c+"+"+d+")"))

        // println(List.range(1,8).foldRight("0")((c,d) => "("+c+"+"+d+")"))

        println(foldLeft(List(1, 2, 3))((zs, x) => (x * 2)::zs)(List(1, 2, 3, 4, 5)))
    }
}