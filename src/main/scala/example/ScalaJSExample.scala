package example
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.Math
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random

abstract class Ateroid(val renderer: dom.CanvasRenderingContext2D) {
}

class Player(val renderer: dom.CanvasRenderingContext2D) {
  val (centerHeight, centerWidth) = ((canvas.height / 2), (canvas.height / 2));

  // status
  var dead = 0

  // commands that pressed
  var down = false
  var up = false
  var left = false
  var right = false
  var space = false

  // const style
  val color = "red"
  val height = 30 // px
  val width = 10

  // position
  var (x,y,r) = (centerHeight,centerWidth,0)
  var points = ((0,0), (0,0), (0,0))
  var sinR = math.sin(r)
  var cosR = math.cos(r)

  def move(dr: Double): Unit = {
    if (down ^ up) {
      if (down) move(-9.0)
      if (up) move(9.0)
    }
    r += dr * math.Pi / 180
    r %= math.Pi*2

    countPoints()
  }

  def countPoints(): Unit = {
    sinR = math.sin(r)
    cosR = math.cos(r)

    points =  (x, y) -> 
      (x + width*cosR, y + width*sinR) ->
      (x + height*sinR, y + height*cosR)
  }

  def draw(): Unit = {
    renderer.fillStyle = color
    renderer.beginPath();
    context.moveTo(points._1._1, points._1._2);
    context.lineTo(points._2._1, points._2._2);
    context.lineTo(points._3._1, points._3._2);
    renderer.fill();
    renderer.closePath();
  }
  
  def frame(): Unit = {
    move()
    draw()
  }
}


@JSExport
object ScalaJSExample {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    //setup
    val renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    //variables
    val (centerHeight, centerWidth) = ((canvas.height / 2), (canvas.height / 2));
    val player = new Player(renderer);

    // What frame this is; used to keep track
    // of where the obstacles should be positioned
    var frame = -25
    // List of each obstacle, storing only the Y position of the hole.
    // The X position of the obstacle is calculated by its position in the
    // queue and in the current frame.
    //val obstacles = collection.mutable.ArrayBuffer[Block]()
    var score = 0

    def runLive() = {
      frame += 1

      // render score
      renderer.fillStyle = "black"
      renderer.font = "20px sans-serif"
      renderer.fillText(s"Score: $score", 100, 20)

      // Create new obstacles, or kill old ones as necessary
      //val deadObstacles = obstacles filter (_.x<=0)
      //score += deadObstacles.length
      //obstacles --= deadObstacles
      //if (frame >= 0 && frame % obstacleGap == 0)
        //obstacles += new Block(Random.nextInt(5)-2, Random.nextInt(5)-2, Random.nextInt(5)-2, center, renderer, rightBorder)


      // Render obstacles, and check for collision
      //renderer.fillStyle = "darkblue"
      //obstacles.foreach(_.draw(frame))
      //if (obstacles.map(_.intersect(player.fullCoords(1))).contains(true) || 
        //obstacles.map(_.intersect(player.fullCoords(2))).contains(true))
        //dead = 50;

      // Render player
      player.draw();
    }

    def runDead() = {
      score = 0
      frame = -50
      //obstacles.clear
      renderer.fillStyle = "darkred"
      renderer.font = "50px sans-serif"
      renderer.textAlign = "center"
      renderer.textBaseline = "middle"
      renderer.fillText("Game Over", canvas.width / 2, canvas.height / 2)
    }

    def run() = {
      renderer.clearRect(0, 0, canvas.width, canvas.height)
      if (dead > 0) runDead()
      else runLive()
    }

    dom.window.setInterval(run _, 20)

    dom.window.onkeydown = (e: dom.KeyboardEvent) => {
      e.keyCode match {
        case 1094 | 119 | 83 | 40 =>
          player.down = true;
        case 1099 | 115 | 87 | 38 =>
          player.up = true;
        case _ => 
          println(s"nothing: ${e.keyCode}")
      }
    }
    dom.window.onkeyup = (e: dom.KeyboardEvent) => {
      e.keyCode match {
        case 1094 | 119 | 83 | 40 =>
          player.down = false;
        case 1099 | 115 | 87 | 38 =>
          player.up = false;
        case _ => 
          println(s"nothing: ${e.keyCode}")
      }
    }
  }
}
