package example
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.Math
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random


@JSExport
object ScalaJSExample {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    //setup
    val renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = 400

    renderer.font = "50px sans-serif"
    renderer.textAlign = "center"
    renderer.textBaseline = "middle"

    //variables
    val obstacleGap = 200 // Gap between the approaching obstacles

    var playerR = 0.0                 // Y velocity of the player
    // Whether the player is dead or not;
    // 0 means alive, >0 is number of frames before respawning
    var dead = 0
    // What frame this is; used to keep track
    // of where the obstacles should be positioned
    var frame = -50
    // List of each obstacle, storing only the Y position of the hole.
    // The X position of the obstacle is calculated by its position in the
    // queue and in the current frame.
    val obstacles = collection.mutable.Queue.empty[Int]
    val center = (canvas.height / 2);

    def runLive() = {
      frame += 2

      // Create new obstacles, or kill old ones as necessary
      if (frame >= 0 && frame % obstacleGap == 0)
        obstacles.enqueue(Random.nextInt(5) - 2)
      if (obstacles.length > 7){
        obstacles.dequeue()
        frame -= obstacleGap
      }

      // Render obstacles, and check for collision
      renderer.fillStyle = "darkblue"
      for((barY, i) <- obstacles.zipWithIndex){
        // Where each obstacle appears depends on what frame it is.
        // This is what keeps the obstacles moving to the left as time passes.
        val holeX = i * obstacleGap - frame + canvas.width // just movint it over time
        val top = center + (barY * 20)

        renderer.fillRect(holeX, top, 5, 20)

        // Kill the player if he hits some obstacle
        //if (math.abs(holeX - canvas.width/2) < 5 &&
          //math.abs(holeY - playerY) > holeSize){
            //dead = 50
          //}
      }

      // Render player
      val playerCirclesGap = 50;
      playerR %= 360
      val cosR = Math.cos(playerR);
      val sinR = Math.sin(playerR);
      val first = (center + cosR * playerCirclesGap, center + sinR * playerCirclesGap);
      val second = (center - cosR * playerCirclesGap, center - sinR * playerCirclesGap);
      val radious = 3;
      renderer.fillStyle = "blue"
      renderer.beginPath();
      renderer.arc(first._1, first._2, radious, 0, Math.PI*2, true); 
      renderer.fill();
      renderer.closePath();

      renderer.fillStyle = "red"
      renderer.beginPath();
      renderer.arc(second._1, second._2, radious, 0, Math.PI*2, true); 
      renderer.fill();
      renderer.closePath();
    }


    def runDead() = {
      playerR = 0
      frame = -50
      obstacles.clear()
      dead -= 1
      renderer.fillStyle = "darkred"
      renderer.fillText("Game Over", canvas.width / 2, canvas.height / 2)
    }


    def run() = {
      renderer.clearRect(0, 0, canvas.width, canvas.height)
      if (dead > 0) runDead()
      else runLive()
    }

    dom.window.setInterval(run _, 20)

    dom.window.onkeypress = (e: dom.KeyboardEvent) => {
      e.keyCode match {
        case 1094 | 119=>
          //println("up")
          playerR -= 3;
        case 1099 | 115=>
          //println("down")
          playerR += 3;
        case _ => 
          println(s"nothing: ${e.keyCode}")
      }
    }
  }
}
