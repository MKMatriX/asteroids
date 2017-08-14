package example
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.Math
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random

@JSExport
object ScalaJSExample {
  var bodies:List[Body] = List()
  var MAX_X = 0
  var MAX_Y = 0

  abstract class Body(
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

    var alive = true
    def isAlive (): Boolean = alive

    var sinR = math.sin(r)
    var cosR = math.cos(r)

    bodies = bodies :+ this

    def frame (): Unit = {
      r += vr * math.Pi / 180
      r %= math.Pi*2
      x += vx
      y += vy

      if (x < 0) x += MAX_X
      if (y < 0) y += MAX_Y
      x %= MAX_X
      y %= MAX_Y

      draw()
    }

    def inField (): Boolean = {
      x > 0 && y > 0 && x < MAX_X && y < MAX_Y
    }

    def draw(): Unit = {}

    def collideBody(body: Body): Unit = {}
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
    ) extends Body (xInit, yInit, rInit, vxInit, vyInit, vrInit) {

    val MIN_RADIUS = 4

    override def draw(): Unit = {
      renderer.beginPath()
      renderer.arc(x, y, radius, 0, 2 * math.Pi, false)
      renderer.fillStyle = "black"
      renderer.fill()
    }

    override def collideBody(body: Body): Unit = {
      body match {
        case bullet:Bullet => checkBullet(bullet)
        case asteroid:Circle => {
          if (collideCircle(asteroid)) {
            if (alive && asteroid.alive) {
              new Circle(
                x + (asteroid.x - x)/2.0,
                y + (asteroid.y - y)/2.0,
                vx + (asteroid.vx - vx)/2.0,
                vy + (asteroid.vy - vy)/2.0,
                radius + asteroid.radius,
                renderer,
                canvas
              )
              alive = false
              asteroid.alive = false
            }
          }
        }
        case _ => {}
      }
    }

    def collide(p: (Int,Int)): Boolean = {
      math.pow(x-p._1,2) + math.pow(y-p._2,2) < math.pow(radius,2)
    }

    def collideCircle(asteroid: Circle): Boolean = {
      math.pow(x-asteroid.x,2) + math.pow(y-asteroid.y,2) < math.pow(radius + asteroid.radius,2)
    }

    def checkBullet(bullet: Bullet): Unit = {
      if (collide((math.floor(bullet.x).toInt, math.floor(bullet.y).toInt))) {
        bullet.alive = false

        if (radius > 8) {
          alive = false
          val firstDelta = Random.nextDouble()*4 - 2
          val secontDelta = Random.nextDouble()*4 - 2

          new Circle(
            x + firstDelta,y + secontDelta,
            vx * firstDelta, vy * secontDelta,
            (radius / 2).toInt,
            renderer,
            canvas
          )
          new Circle(
            x + firstDelta,y + secontDelta,
            vx * firstDelta, vy * secontDelta,
            (radius / 2).toInt - 1,
            renderer,
            canvas
          )
        } else {
          radius -= 1
          if (radius <= MIN_RADIUS) {
            alive = false
          }
        }
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
    ) extends Body (xInit, yInit, rInit, vxInit, vyInit, vrInit) {
    val (centerHeight, centerWidth) = ((canvas.height / 2), (canvas.width / 2));

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
    countPoints()

    var bullets: List[Bullet] = List()

    def testPoint(p: (Int,Int)): Boolean = {
      p._1 > 0 && p._2 > 0 && p._1 < MAX_X && p._2 < MAX_Y
    }

    def collide(asteroids: List[Circle]): Unit = {
     alive &= !asteroids.exists(asteroid => {
       points.foldLeft(false)((acc:Boolean, point: (Int, Int)) => {
        acc || asteroid.collide(point)
       })
     })
    }

    def move(): Unit = {
      // handle rotation
      var dr = 0.0
      if (left ^ right) {
        if (left) dr = -0.4
        if (right) dr = 0.4
      }
      vr += dr
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
    }

    def restart(): Unit = { // return to initial stats
      alive = true;
      x = centerWidth
      vx = 0.0
      y = centerHeight
      vy = 0.0
      r = 0.0
      vr = 0.0
      countPoints()
    }

    def shoot(): Unit = {
      val speed = 3.0
      new Bullet(
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
      if (space) shoot()
      super.frame()
      countPoints()
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
    ) extends Body (xInit, yInit, rInit, vxInit, vyInit, vrInit) {

    vx += math.cos(r) * speed
    vy += math.sin(r) * speed

    var energy = 999

    override def frame (): Unit = {
      energy -= 1
      alive &= energy > 0
      super.frame()
    }

    override def draw (): Unit = {
      renderer.beginPath()
      renderer.arc(x, y, 1, 0, 2 * math.Pi, false)
      renderer.fillStyle = "black"
      renderer.fill()
    }
  }

  def addAsteroid(
    renderer: dom.CanvasRenderingContext2D,
    canvas: html.Canvas
  ): Unit = {
    val asterodsAim = 4.0
    val asterodsAimX = MAX_X / asterodsAim
    val asterodsAimY = MAX_Y / asterodsAim

    val side = Random.nextInt(3)
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

    val asteroidSpeed = 80 + Random.nextDouble()*200

    val asteroidVX = (MAX_X/2 - asteroidX + Random.nextDouble()*asterodsAimX - asterodsAimX/2) / asteroidSpeed
    val asteroidVY = (MAX_Y/2 - asteroidY + Random.nextDouble()*asterodsAimY - asterodsAimY/2) / asteroidSpeed

    new Circle(
      asteroidX, asteroidY,
      asteroidVX, asteroidVY,
      Random.nextInt(20) + 4,
      renderer,
      canvas
    )
  }

  @JSExport
  def main(canvas: html.Canvas): Unit = {
    //setup
    val renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = dom.window.innerHeight.toInt

    MAX_X = canvas.width
    MAX_Y = canvas.height

    //variables
    val player = new Player(renderer, canvas)

    var frame = -25
    var score = 0
    var restart = true;
    val obstacleGap = 40

    def runLive() = {
      frame += 1

      // render score
      renderer.fillStyle = "black"
      renderer.font = "20px sans-serif"
      renderer.fillText(s"Score: $score", 100, 20)

      // Create new asteroids
      if (frame >= 0 && frame % obstacleGap == 0) {
        addAsteroid(renderer, canvas)
      }

      val asteroids = bodies.collect { case a: Circle => a }.asInstanceOf[List[Circle]]
      val bullets = bodies.collect { case a: Bullet => a }.asInstanceOf[List[Bullet]]

      player.collide(asteroids)

      // bullets.foreach(bullet => asteroids
        // .map(asteroid => asteroid.checkBullet(bullet))
      // )

      bodies.foreach(firstBody => bodies.filter(_ != firstBody).foreach(_.collideBody(firstBody)))

      score += asteroids.filter(!_.isAlive()).length

      bodies = bodies
        // .filter(_.inField())
        .filter(_.isAlive())

      // render all
      bodies.map(_.frame())
    }

    def restartTimer(): Unit = {
      restart = false
      def tmp: Unit = {
        player.restart()
        restart = true
        score = 0
        bodies = List(player)
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
      // bodies = bodies.collect {case a: Player => a}
    }

    def run() = {
      renderer.clearRect(0, 0, canvas.width, canvas.height)
      if (player.alive) runLive()
      else runDead()
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
