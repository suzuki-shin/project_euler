import scala.io.StdIn

object Main {
  def main(args:Array[String]) = {
    val in = StdIn.readLine.split(",")

    println( q(in) )
  }

  def q(names:Array[String]):Int = {
    def score(name:String, i:Int):Int = {
      name.map(c => (c.toLower.asDigit - 9)).sum * (i + 1)
    }

    names.sorted.zipWithIndex.map({case(name, i) => score(name.replace("\"",""), i)}).sum
  }
}
