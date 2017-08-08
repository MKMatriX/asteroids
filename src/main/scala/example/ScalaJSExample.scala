package example
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.Math
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random


abstract class body(
  var x: Double = 0.0,
  var y: Double = 0.0,
  var r: Double = 0.0,
  var vx: Double = 0.0,
  var vy: Double = 0.0,
  var vr: Double = 0.0
  ) {
  var sinR = math.sin(r)
  var cosR = math.cos(r)

  def frame (): Unit = {
    // handle rotation
    r += vr * math.Pi / 180
    r %= math.Pi*2
    sinR = math.sin(r)
    cosR = math.cos(r)

    // handle acceliration
    x += vx
    y += vy

    this.draw()
  }

  def draw(): Unit = {}
}

class Circle (
    x: Double,
    y: Double,
    r: Double = 0.0,
    vx: Double = 0.0,
    vy: Double = 0.0,
    vr: Double = 0.0,
    radius: Double = 0.0,
    val renderer: dom.CanvasRenderingContext2D,
    canvas: html.Canvas
  ) extends body (x, y, r, vx, vy, vr) {

  override def draw(): Unit = {
    renderer.beginPath()
    renderer.arc(x, y, radius, 0, 2 * math.Pi, false)
    renderer.fillStyle = "green"
    renderer.fill()
  }
}

/*
class Asteroid extends Circle{
  def draw(): Unit = {
  }
  def frame(): Unit = {
  }
}
*/

class Player(val renderer: dom.CanvasRenderingContext2D, canvas: html.Canvas) {
  val (centerHeight, centerWidth) = ((canvas.height / 2), (canvas.width / 2));

  // status
  var dead = false

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
  var (x, vx, y, vy, r) = (centerWidth.toDouble, 0.0, centerHeight.toDouble, 0.0, 0.0)
  //var points = ((0,0), (0,0), (0,0))
  var points = Array((0,0), (0,0), (0,0))
  var sinR = math.sin(r)
  var cosR = math.cos(r)

  var bullets: List[Bullet] = List()

  def testPoint(p: (Int,Int)): Boolean = {
    p._1 > 0 && p._2 > 0 && p._1 < canvas.width && p._2 < canvas.height
  }

  def move(): Unit = {
    // handle rotation
    var dr = 0.0
    if (left ^ right) {
      if (left) dr = -9.0
      if (right) dr = 9.0
    }
    r += dr * math.Pi / 180
    r %= math.Pi*2
    sinR = math.sin(r)
    cosR = math.cos(r)

    // handle acceliration
    var dv = 0.0
    if (up ^ down) {
      if (up) dv = 0.1
      if (down) dv = -0.1
    }
    vx += -dv*sinR
    vy += dv*cosR
    x += vx
    y += vy

    countPoints()
    checkOutOfBorder()
  }

  def checkOutOfBorder(): Unit = {
    dead = !points.exists(testPoint)
  }

  def die(): Unit = {
    dead = false;
    x = centerWidth
    vx = 0.0
    y = centerHeight
    vy = 0.0
    r = 0.0
    bullets = List()
  }

  def shoot(): Unit = {
    val speed = 1.0
    bullets = bullets :+ new Bullet(
      renderer,
      x,
      y,
      (r + math.Pi/2),
      speed,
      vx,
      vy
    )
  }

  def countPoints(): Unit = {
    var w = (width * cosR / 2).toInt
    var h = (width * sinR / 2).toInt
    var yw = (height * sinR / -3).toInt
    var yh = (height * cosR / 3).toInt

    points(0) = (x.toInt- w - yw, y.toInt - h - yh)
    points(1) = (x.toInt + w - yw, y.toInt + h - yh)
    points(2) = (x.toInt + yw * 2, y.toInt + yh * 2)
  }

  def draw(): Unit = {
    renderer.beginPath();
    renderer.moveTo(points(0)._1, points(0)._2);
    renderer.lineTo(points(1)._1, points(1)._2);
    renderer.lineTo(points(2)._1, points(2)._2);
    renderer.closePath();
    renderer.fillStyle = color
    renderer.fill();
  }
  
  def frame(): Unit = {
    move()
    draw()
    bullets = bullets.filter(bullet => testPoint(bullet.x.toInt -> bullet.y.toInt))
    bullets.map(_.frame())
  }
}

class Bullet (
  val renderer: dom.CanvasRenderingContext2D,
  var x: Double,
  var y: Double,
  var r: Double,
  var speed: Double = 1.0,
  val parentVx: Double,
  val parentVy: Double
  ) {
  val vx = math.cos(r) * speed + parentVx
  val vy = math.sin(r) * speed + parentVy

  def frame (): Unit = {
    x += vx
    y += vy
    draw()
  }
  def draw (): Unit = {
    renderer.beginPath()
    renderer.arc(x, y, 3, 0, 2 * math.Pi, false)
    renderer.fillStyle = "green"
    renderer.fill()
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
    val (centerHeight, centerWidth) = ((canvas.height / 2), (canvas.width / 2));
    val player = new Player(renderer, canvas);

    // What frame this is; used to keep track
    // of where the obstacles should be positioned
    var frame = -25
    // List of each obstacle, storing only the Y position of the hole.
    // The X position of the obstacle is calculated by its position in the
    // queue and in the current frame.
    //val obstacles = collection.mutable.ArrayBuffer[Block]()
    var score = 0
    var restart = true;

    var asteroids:List[Circle] = List()
    val obstacleGap = 400

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
      if (frame >= 0 && frame % obstacleGap == 0)
        asteroids = asteroids :+ new Circle(
          10.0,
          20.0,
          0.0,
          2.0,
          3.0,
          0.0,
          8.0,
          renderer,
          canvas
        )
        //obstacles += new Block(Random.nextInt(5)-2, Random.nextInt(5)-2, Random.nextInt(5)-2, center, renderer, rightBorder)


      // Render obstacles, and check for collision
      //renderer.fillStyle = "darkblue"
      //obstacles.foreach(_.draw(frame))
      //if (obstacles.map(_.intersect(player.fullCoords(1))).contains(true) || 
        //obstacles.map(_.intersect(player.fullCoords(2))).contains(true))
        //dead = 50;

      // Render player
      player.frame();

      asteroids map(_.frame())
    }

    def restartTimer(): Unit = {
      restart = false
      def tmp: Unit = {
        player.die()
        restart = true
      }
      dom.window.setTimeout(() => tmp, 2000)
    }
    def runDead() = {
      score = 0
      frame = -50
      if (restart) {
        restartTimer()
      }
      //obstacles.clear
      renderer.fillStyle = "darkred"
      renderer.font = "50px sans-serif"
      renderer.textAlign = "center"
      renderer.textBaseline = "middle"
      renderer.fillText("Game Over", canvas.width / 2, canvas.height / 2)
    }

    def run() = {
      renderer.clearRect(0, 0, canvas.width, canvas.height)
      if (player.dead) runDead()
      else runLive()
    }

    dom.window.setInterval(run _, 20)

    dom.window.onkeydown = (e: dom.KeyboardEvent) => {
      e.keyCode match {
        case 68 | 39 =>
          player.right = true;
        case 65 | 37 =>
          player.left = true;
        case 1094 | 119 | 83 | 40 =>
          player.down = true;
        case 1099 | 115 | 87 | 38 =>
          player.up = true;
        case 32 =>
          player.shoot()
        case _ => 
          println(s"nothing: ${e.keyCode}")
      }
    }
    dom.window.onkeyup = (e: dom.KeyboardEvent) => {
      e.keyCode match {
        case 68 | 39 =>
          player.right = false;
        case 65 | 37 =>
          player.left = false;
        case 1094 | 119 | 83 | 40 =>
          player.down = false;
        case 1099 | 115 | 87 | 38 =>
          player.up = false;
        case 32 =>
          null
        case _ => 
          println(s"nothing: ${e.keyCode}")
      }
    }
  }
}
