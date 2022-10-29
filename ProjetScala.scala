import java.io.FileNotFoundException

object TondeuseAuto {
   /*
   La Classe Tondeuse permet de recuperer la position initiale @position, la direction initiale @direction, les coordonnees limites de la pelouse @limit, l'id de la tendeuse @id et finalement les instructions pour avancer 'A' tourner à droite 'D' ou tourner à gauche 'G'
   Il faut declarer un nouvel object Tondeuse et fournir tous les @params puis utiliser la methode start() pour obtenir le resultat final : la position et la direction finale de la tondeuse
   */
   class Tondeuse(ID : Int, pos : List[Int] , limit : List[Int] , dir: String , inst : String){
      var id : Int = ID
      var position : List[Int] = pos
      var direction : Char = dir.charAt(0)
      var instructions : String = inst
      // cette variable permet de choisir si la direction est suivant l'axe x ('E' et 'W') dans ce cas on recupere 0, ou y ('N' et 'S') dans ce cas on recupere 1, elle permet aussi de savoir si on avance ou bien on recule sur un axe donne ('N' et 'E') -> +1 et ('S' et 'W') -> -1 , finallement un Map permet de determiner la prochaine direction suivant l'@instructuion par exemple dans le cas où la direction actuelle est 'N' et l'instruction 'G' -> DIRECTION('N')(2)('G') -> nous obtiendrons 'W' 

      val DIRECTIONS = Map(
         'N' -> List( 1, 1, Map('G' -> 'W' , 'D' -> 'E') ), 
         'E' -> List( 0, 1, Map('G' -> 'N' , 'D' -> 'S') ) , 
         'S' -> List( 1,-1, Map('G' -> 'E' , 'D' -> 'W') ), 
         'W' -> List( 0,-1, Map('G' -> 'S' , 'D' -> 'N') ) 
                              )
      // La methode @checkLimit permet de verifier si la position @position actuelle de la Tondeuse est toujours à l'interieur de la pelouse                             
      def checkLimit(v : Int , p : Int) = {
         if (p == 1){
         if ( ( ( position(1) + v) <= limit(1) ) & ( (position(1) + v) >= 0) )  
         {true} else {false}
         } else {
            if ( ( ( position(0) + v) <= limit(0) ) & ( (position(0) + v) >= 0) ) 
         {true} else {false}
         }
      }
      //La methode @move permet de suivre une seule instruction @instruction et d'avancer ainsi la @position de la tondeuse, de la tourner ou de rester sur place
      def move(instruction : Char) = {
         if (instruction == 'A') {
            var p : Int = DIRECTIONS(direction)(0).toString.toInt
               var v : Int = if ( checkLimit(DIRECTIONS(direction)(1).toString.toInt , p) == true )
               {  DIRECTIONS(direction)(1).toString.toInt } 
               else { 0}
               position = position.patch(p , Seq(position(p) + v) , 1)
               }
         else 
            direction =   DIRECTIONS(direction)(2).asInstanceOf[Map[Char, Char]](instruction)
            }
      
      // la methode @start permet de parcourir toutes les @instructions et d'afficher la position et la direction finale de la tondeuse
      def start() = {
         for(instruction <- instructions){ move(instruction)}
         println("Tondeuse " + id.toString + ": " + position(0).toString + " " +  position(1).toString + " " + direction )
      } 
   }

   def main(args: Array[String]) {
      // Nous verifions d'abord l'existance du fichier et aussi la conformite de la structures de donnees
         try {
            // Lecture du fichier et extraction des donnees
         val data = scala.io.Source.fromFile("input.txt").getLines()
         val commands = (for (elm <- data) yield elm.split(" ").toList ).toList
         // Recuperation des coordonnees limites de la pelouse
         val maxSize = commands(0).map(_.toInt)
         // l'id de la tondeuse
         var j : Int = 1
         // Cette boucle permet de creer à chque fois un objet Tondeuse et d'executer la methode start
         for (i <-1 to commands.length-1 by 2){
            var t = new Tondeuse(j , commands(i).slice(0,2).map(_.toInt), maxSize , commands(i)(2), commands(i+1)(0) )
            j = j + 1
            t.start()
            }         
         } catch {
         case x: FileNotFoundException => println("Prière de verifier le fichier \"input.txt\" ou le format des donnees.")
         }
         
   }
}