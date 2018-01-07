package kayttoliittyma
import mallinnus.{World, Camera, Wall}
import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.event._
import scala.swing.Action
import java.awt.{Color}
import javax.swing.UIManager
import scala.math.{exp, Pi, sin}
import org.ejml.simple._


object UI extends SimpleSwingApplication{
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  val width = 1280
  val height = 720    
  val world = new World()
  val c = new Camera(world.surface, width, height)
  
  val eteen       = new Button("eteen")
  val taakse      = new Button("taakse")
  val vasen       = new Button("vasen")
  val oikea       = new Button("oikea")
  val kaannyVasen = new Button("käänny vasempaan")
  val kaannyOikea = new Button("käänny oikeaan")
  
  val canvas = new Panel {
    this.preferredSize = new Dimension(width, height)
    
    /**
     * canvas-paneelin paintComponent-metodi tekee ohjelmassa varsinaisen piirtämisen.
     * Metodi käy läpi maailman seinät yksi kerrallaan. Seiniltä haetaan niiden 2-ulotteiset koordinaatit,
     * jotka on saatu projektiosta. Nämä koordinaatit muunnetaan Graphics2D:n vaatimaan muotoon, 
     * ja tämän jälkeen seinä piirretään käyttäen apuna Graphics2D:n fillPolygon-metodia.
     * Myös seinien törmäyspisteet voidaan piirtää piste kerrallaan.
     */
    override def paintComponent(g: Graphics2D) {
      val bgCorners = getSkyAndFloorCornerPoints()
      g.setColor(new Color(100,100,255))
      g.fillPolygon(bgCorners._1, bgCorners._2, 4)  //taivas
      g.setColor(new Color(100,100,100))
      g.fillPolygon(bgCorners._3, bgCorners._4, 4)  //maa/lattia
      
      world.updateTwoDimCoords()
      world.walls.foreach { wall => 
        //seinien piirto
        if (wall.twoDimCoords.isDefined) {
          val coordinates = wall.twoDimCoords.get
          val drawCoordinates = coordinates.map(crd => fromWorldToCanvasConvert(crd))
          val d = wall.getDistance
          val xx = drawCoordinates.map(crd => crd._1).toArray
          val yy = drawCoordinates.map(crd => crd._2).toArray
          val l = coordinates.length
          g.setColor(Color.white)
          g.fillPolygon(xx, yy, l)
          val c = Color.blue  //seinän väri
          /*
           * seinän väri riippuu sen etäisyydestä: apha-arvo kerrotaan etäisyyden
           * perusteella eksponenttifunktion avulla
           */
          g.setColor(new Color(c.getRed, c.getGreen, c.getBlue, ((exp(-d/20)*0.4 + 0.6)*255).toInt))
          g.fillPolygon(xx, yy, l)
        }
        //törmäyspisteiden piirto
        g.setColor(Color.black)
        if (wall.twoDimColPoints.isDefined) {
          val points = wall.twoDimColPoints.get
          points.foreach(p => {
            val drawP = fromWorldToCanvasConvert(p)
            g.fillOval(drawP._1, drawP._2, 5, 5)
          })
        }
      }
    }
  }
    
  /**
   * Palauttaa neljä Arrayta, jotka sisältävät taivasruudun
   * x-koordinaatit ja y-koordinaatit sekä lattiaruudun x-koordinaatit
   * ja y-koordinaatit.
   */
  def getSkyAndFloorCornerPoints(): Tuple4[Array[Int], Array[Int], Array[Int], Array[Int]] = {
    val horizonLine = sin(-c.getAngle) * this.world.surface.x
    val canvasSize = this.canvas.size
    val w = canvasSize.getWidth
    val h = canvasSize.getHeight
    val chh = ((this.c.zmax - horizonLine) / this.c.zlen * h).round.toInt   //canvas horizon height

    val sx: Array[Int] = Array(0, w, w, 0).map(_.toInt)      //sky x
    val sy: Array[Int] = Array(0, 0, chh, chh).map(_.toInt)  //sky y
    val fx = sx                                              //floor x
    val fy: Array[Int] = Array(chh, chh, h, h).map(_.toInt)  //floor y
    (sx, sy, fx, fy)
  }
  
  /**
   * Tämä metodi muuntaa sisäisestä mallista saadut (y, z)-koordinaatit
   * piirtoalueelle sopiviksi (x, y)-koordinaateiksi.
   */
  def fromWorldToCanvasConvert(coords: (Double, Double)): (Int, Int) = {
    val worldWidth = this.c.ylen
    val worldHeight = this.c.zlen
    val canvasSize = this.canvas.size
    val canvasWidth = canvasSize.getWidth
    val canvasHeight = canvasSize.getHeight
    val x = ((coords._1 + this.c.ymax) / worldWidth * canvasWidth).round.toInt
    val y = ((this.c.zmax - coords._2) / worldHeight * canvasHeight).round.toInt
    (x, y)
  }
  
  
  def top = new MainFrame {
    title = "3D-visualisointi"
    
    val buttonPanel = new FlowPanel {
      contents += vasen
      contents += oikea
      contents += eteen
      contents += taakse
      contents += kaannyVasen
      contents += kaannyOikea
    }
    
    var movementFlag: Int = 0  //Lippu joka vaihtuu sen mukaan mihin ollaan liikkumassa
    
    val contentPanel = new BorderPanel {
      layout(canvas) = Center
      layout(buttonPanel) = South
      focusable = true
      requestFocus
      
      import java.awt.event._
      listenTo(kaannyVasen)
      listenTo(kaannyOikea)
      listenTo(eteen)
      listenTo(taakse)
      listenTo(vasen)
      listenTo(oikea)
      listenTo(this.keys)
      
      val listener = new ActionListener(){
        /**
         * actionPerformed suoritetaan jatkuvasti timerin mukaan. Tämä metodi päivittää ohjelman
         * tilan ja kutsuu uudelleen piirtämistä.
         */
        def actionPerformed(e : java.awt.event.ActionEvent) = {
          movementFlag match {
            case 0 =>
            case 1 =>  world.applyTransform(world.transforms.translateXHorizontal(0.1, c.getAngle))
            case 2 =>  world.applyTransform(world.transforms.translateXHorizontal(-0.1, c.getAngle))
            case 3 =>  world.applyTransform(world.transforms.translateY(-0.1))
            case 4 =>  world.applyTransform(world.transforms.translateY(0.1))
            case 5 =>  if (world.applyTransform(world.transforms.translateZVertical(0.1, c.getAngle)))                     c.lift(0.1)
            case 6 =>  if (c.canLower(0.1) && world.applyTransform(world.transforms.translateZVertical(-0.1, c.getAngle))) c.lower(0.1)
            case 7 =>  world.applyTransform(world.transforms.rotateZHorizontal(Pi/160, c.getAngle))
            case 8 =>  world.applyTransform(world.transforms.rotateZHorizontal(-Pi/160, c.getAngle))
            case 9 =>  if (world.applyTransform(world.transforms.rotateY(Pi/160)))   c.rotate(Pi/160)
            case 10 => if (world.applyTransform(world.transforms.rotateY(-Pi/160)))  c.rotate(-Pi/160)
          }
          world.updateTwoDimCoords()
          world.updateTwoDimColPoints()
          repaint()
        }
      }
      
      val timer = new javax.swing.Timer(6, listener)
      timer.start()
      
      //Ohjelman tilaa voidaan muuttaa myös käyttöliittymän nappuloiden avulla
      this.reactions += {
        case ButtonClicked(component) => {
            if (component == kaannyVasen) world.applyTransform(world.transforms.smallRotLeft)
            if (component == kaannyOikea) world.applyTransform(world.transforms.smallRotRight)
            if (component == eteen)       world.applyTransform(world.transforms.translateX(1))
            if (component == taakse)      world.applyTransform(world.transforms.translateX(-1))
            if (component == vasen)       world.applyTransform(world.transforms.translateY(-1))
            if (component == oikea)       world.applyTransform(world.transforms.translateY(1))
          world.updateTwoDimCoords()
          repaint
          requestFocus
        }
        //Näppäimen painallus muuttaa movementFlagin tilaa. Lipun käyttö mahdollistaa sulavan liikkumisen.
        case KeyPressed(_, key, _, _) => {
            if (key == Key.W)       movementFlag = 1
            if (key == Key.S)       movementFlag = 2
            if (key == Key.A)       movementFlag = 3
            if (key == Key.D)       movementFlag = 4
            if (key == Key.Space)   movementFlag = 5
            if (key == Key.Shift)   movementFlag = 6
            if (key == Key.Left)    movementFlag = 7
            if (key == Key.Right)   movementFlag = 8
            if (key == Key.Up)      movementFlag = 9
            if (key == Key.Down)    movementFlag = 10
            if (key == Key.R)       {
              world.resetWorld()
              c.reset()
            }
        }
        case KeyReleased(_, key, _, _) =>   movementFlag = 0
      }
    }
    
    contents = contentPanel
    menuBar = new MenuBar {
      contents += new Menu("Tiedosto") {
        contents += new MenuItem(new Action("Avaa") {
          def apply() {
            world.loadWorld()
            c.reset()
          }
        })
        contents += new MenuItem(new Action("Sulje") {
          def apply() {
            world.clearWorld()
            c.reset()
          }
        })
      }
    }
    
    this.pack()
    
  }
 
}