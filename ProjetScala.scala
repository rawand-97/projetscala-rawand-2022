object TondeuseAuto {

   class Tondeuse(id : Int, pos : List[Int] , limit : List[Int] , direction: String , instructions : String){
      var id_ : Int = id
      var pos_ : List[Int] = pos
      var direction_ : Char = direction.charAt(0)
      var instructions_ : String = instructions
      var limit_ : List[Int] = limit
      val DIRECTIONS = Map(
         'N' -> List( 1, 1, Map('G' -> 'W' , 'D' -> 'E') ), 
         'E' -> List( 0, 1, Map('G' -> 'N' , 'D' -> 'S') ) , 
         'S' -> List( 1,-1, Map('G' -> 'E' , 'D' -> 'W') ), 
         'W' -> List( 0,-1, Map('G' -> 'S' , 'D' -> 'N') ) 
                              )
      def check_limit(v : Int , p : Int) = {
         if (p == 1){
         if ( ( ( pos_(1) + v) <= limit_(1) ) & ( (pos_(1) + v) >= 0) )  
         {true} else {false}
         } else {
            if ( ( ( pos_(0) + v) <= limit_(0) ) & ( (pos_(0) + v) >= 0) ) 
         {true} else {false}
         }
      }
      def move(instruction : Char) = {
         if (instruction == 'A') {
            var p : Int = DIRECTIONS(direction_)(0).toString.toInt
               var v : Int = if ( check_limit(DIRECTIONS(direction_)(1).toString.toInt , p) == true )
               {  DIRECTIONS(direction_)(1).toString.toInt } 
               else { 0}
               pos_ = pos_.patch(p , Seq(pos_(p) + v) , 1)
               }
         else 
            direction_ =   DIRECTIONS(direction_)(2).asInstanceOf[Map[Char, Char]](instruction)
            }
      def start() = {
         for(instruction <- instructions_){ move(instruction)}
         println("Tondeuse " + id_.toString + ": " + pos_(0).toString + " " +  pos_(1).toString + " " + direction_ )
      } 
   }

   def main(args: Array[String]) {

         val data = scala.io.Source.fromFile("input.txt").getLines()
         val commands = (for (elm <- data) yield elm.split(" ").toList ).toList
         val maxSize = commands(0).map(_.toInt)
         for (i <-1 to commands.length-1 by 2) println(commands(i), commands(i+1))
         var j : Int = 1
         for (i <-1 to commands.length-1 by 2){
            var t = new Tondeuse(j , commands(i).slice(0,2).map(_.toInt), maxSize , commands(i)(2), commands(i+1)(0) )
            j = j + 1
            t.start()
            }
   }
}