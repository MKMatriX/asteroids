package example
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.Math
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random


abstract class body(
  xInit: Double,
  yInit: Double,
  rInit: Double,
  vxInit: Double,
  vyInit: Double,
  vrInit: Double
  ) {
  var x = xInit
  var y = yInit
  var r = rInit
  var vx = vxInit
  var vy = vyInit
  var vr = vrInit

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

    draw()
  }

  def draw(): Unit = {}
}

class Circle (
    xInit: Double,
    yInit: Double,
    vxInit: Double,
    vyInit: Double,
    var radius: Int,
    var renderer: dom.CanvasRenderingContext2D,
    var canvas: html.Canvas,
    rInit: Double = 0.0,
    vrInit: Double = 0.0
  ) extends body (xInit, yInit, rInit, vxInit, vyInit, vrInit) {

  var alive = true
  val MIN_RADIUS = 4

  override def draw(): Unit = {
    renderer.beginPath()
    renderer.arc(x, y, radius, 0, 2 * math.Pi, false)
    renderer.fillStyle = "black"
    renderer.fill()
  }

  def inField(): Boolean = {
    x > 0 && x < canvas.width && y > 0 && y < canvas.height
  }

  def isAlive(): Boolean = alive

  def collide(p: (Int,Int)): Boolean = {
    math.pow(x-p._1,2) + math.pow(y-p._2,2) < math.pow(radius,2)
  }

  def checkBullet(bullet: Bullet): Unit = {
    if (collide((math.floor(bullet.x).toInt, math.floor(bullet.y).toInt))) {
      radius -= 3
      if (radius <= MIN_RADIUS) {
        alive = false
      }
      bullet.alive = false
    }
  }
}


class Player (
    var renderer: dom.CanvasRenderingContext2D,
    var canvas: html.Canvas,
    xInit: Double = 0.0,
    yInit: Double = 0.0,
    rInit: Double = 0.0,
    vxInit: Double = 0.0,
    vyInit: Double = 0.0,
    vrInit: Double = 0.0
  ) extends body (xInit, yInit, rInit, vxInit, vyInit, vrInit) {
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
  x = centerWidth.toDouble
  y = centerHeight.toDouble
  var points = Array((0,0), (0,0), (0,0))

  var bullets: List[Bullet] = List()

  def testPoint(p: (Int,Int)): Boolean = {
    p._1 > 0 && p._2 > 0 && p._1 < canvas.width && p._2 < canvas.height
  }

  def collide(asteroids: List[Circle]): Unit = {
    dead |= asteroids.exists(asteroid => {
      points.foldLeft(false)(
        (acc: Boolean, p:(Int, Int)) => acc || asteroid.collide(p)
      )
    })

    bullets.foreach(bullet => asteroids
      .map(asteroid => asteroid.checkBullet(bullet))
    )
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
    val speed = 3.0
    bullets = bullets :+ new Bullet(
      points(2)._1,
      points(2)._2,
      (r + math.Pi/2),
      vx,
      vy,
      0.0,
      speed,
      renderer
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

  override def draw(): Unit = {
    renderer.beginPath();
    renderer.moveTo(points(0)._1, points(0)._2);
    renderer.lineTo(points(1)._1, points(1)._2);
    renderer.lineTo(points(2)._1, points(2)._2);
    renderer.closePath();
    renderer.fillStyle = color
    renderer.fill();
  }
  
  override def frame(): Unit = {
    move()
    draw()
    if (space) shoot()
    bullets = bullets.filter(bullet => testPoint(bullet.x.toInt -> bullet.y.toInt)).filter(_.isAlive)
    bullets.map(_.frame())
  }
}

class Bullet (
  xInit: Double,
  yInit: Double,
  rInit: Double,
  vxInit: Double,
  vyInit: Double,
  vrInit: Double,
  speed: Double = 1.0,
  val renderer: dom.CanvasRenderingContext2D
  ) extends body (xInit, yInit, rInit, vxInit, vyInit, vrInit) {

  vx += math.cos(r) * speed
  vy += math.sin(r) * speed

  var alive = true

  def isAlive():Boolean = alive

  // def frame (): Unit = {
    // x += vx
    // y += vy
    // draw()
  // }

  override def draw (): Unit = {
    renderer.beginPath()
    renderer.arc(x, y, 1, 0, 2 * math.Pi, false)
    renderer.fillStyle = "black"
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
    canvas.height = dom.window.innerHeight.toInt

    val MAX_X = canvas.width
    val MAX_Y = canvas.height

    //variables
    val (centerHeight, centerWidth) = ((canvas.height / 2), (canvas.width / 2));
    val player = new Player(renderer, canvas)

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
    val obstacleGap = 40

    val asterodsAim = 4.0
    val asterodsAimX = MAX_X / asterodsAim
    val asterodsAimY = MAX_Y / asterodsAim

    def runLive() = {
      frame += 1

      // render score
      renderer.fillStyle = "black"
      renderer.font = "20px sans-serif"
      renderer.fillText(s"Score: $score", 100, 20)

      // Create new asteroids
      if (frame >= 0 && frame % obstacleGap == 0) {
        var side = Random.nextInt(3)
        var asteroidX = 0.0
        var asteroidY = 0.0
        side match {
          case 0 => {
            asteroidX = 1.0
            asteroidY = Random.nextInt(MAX_Y).toDouble
          }
          case 2 => {
            asteroidX = MAX_X - 1.0
            asteroidY = Random.nextInt(MAX_Y).toDouble
          }
          case 1 => {
            asteroidX = Random.nextInt(MAX_X).toDouble
            asteroidY = 1.0
          }
          case 3 => {
            asteroidX = Random.nextInt(MAX_Y).toDouble
            asteroidY = MAX_Y - 1.0
          }
        }

        var asteroidSpeed = 80 + Random.nextDouble()*200

        var asteroidVX = (MAX_X/2 - asteroidX + Random.nextDouble()*asterodsAimX - asterodsAimX/2) / asteroidSpeed
        var asteroidVY = (MAX_Y/2 - asteroidY + Random.nextDouble()*asterodsAimY - asterodsAimY/2) / asteroidSpeed


        asteroids = asteroids :+ new Circle(
          asteroidX,
          asteroidY,
          asteroidVX,
          asteroidVY,
          Random.nextInt(20) + 4,
          renderer,
          canvas
        )
      }


      // Render player
      player.frame()

      player.collide(asteroids)

      score += asteroids.filter(!_.isAlive()).length

      // render asteroids
      asteroids = asteroids
          .filter(_.inField())
          .filter(_.isAlive())
      asteroids.map(_.frame())
    }

    def restartTimer(): Unit = {
      restart = false
      def tmp: Unit = {
        player.die()
        restart = true
	score = 0
      }
      dom.window.setTimeout(() => tmp, 3000)
    }
    def runDead() = {
      renderer.fillStyle = "darkred"
      renderer.font = "50px sans-serif"
      renderer.textAlign = "center"
      renderer.textBaseline = "middle"
      renderer.fillText(s"Game Over score: ${score}", MAX_X / 2, MAX_Y / 2)

      frame = -50
      if (restart) {
        restartTimer()
      }
      asteroids = List()
      //obstacles.clear
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
          player.up = true
        case 32 =>
          player.space = true
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
          player.space = false
        case _ => 
          println(s"nothing: ${e.keyCode}")
      }
    }
  }
}
