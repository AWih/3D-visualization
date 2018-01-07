package mallinnus
import org.ejml.simple._
import scala.math.{sin, cos, tan, Pi}

/**
 * Transforms-yksittäisolio sisältää visualisoinnissa käytettävät matriisit,
 * joiden avulla seiniä liikutetaan ympäristössä.
 */
object Transforms {
  def rotateZ(angle: Double): SimpleMatrix = {
    new SimpleMatrix( Array(Array[Double](cos(angle), -sin(angle), 0, 0),
                            Array[Double](sin(angle), cos(angle),  0, 0),
                            Array[Double](0,          0,           1, 0),
                            Array[Double](0,          0,           0, 1)) )  
    }
  
  def rotateY(angle: Double): SimpleMatrix = {
    new SimpleMatrix( Array(Array[Double](cos(angle),  0, sin(angle), 0),
                            Array[Double](0,           1, 0,          0),
                            Array[Double](-sin(angle), 0, cos(angle), 0),
                            Array[Double](0,           0, 0,          1)) )  
    }
  
  def translateX(distance: Double): SimpleMatrix = {
    new SimpleMatrix( Array(Array[Double](1, 0, 0, -distance),
                            Array[Double](0, 1, 0, 0),
                            Array[Double](0, 0, 1, 0),
                            Array[Double](0, 0, 0, 1)) )  
    }

  def translateY(distance: Double): SimpleMatrix = {
    new SimpleMatrix( Array(Array[Double](1, 0, 0, 0),
                            Array[Double](0, 1, 0, -distance),
                            Array[Double](0, 0, 1, 0),
                            Array[Double](0, 0, 0, 1)) )  
    }  
  
  def translateZ(distance: Double): SimpleMatrix = {
    new SimpleMatrix( Array(Array[Double](1, 0, 0, 0),
                            Array[Double](0, 1, 0, 0),
                            Array[Double](0, 0, 1, -distance),
                            Array[Double](0, 0, 0, 1)) )  
  }
  
  /**
   * Tekee x-suuntaisen translaatiomatriisin siten, että y-suuntaisen kierron vaikutus
   * poistetaan. Tämä luo vaikutelman, että liikutaan maanpinnan suunnassa riippumatta
   * kameran katselukulmasta.
   * @param angle on kameralta saatava katselukulma, joka on kiertokulma y-akselin ympäri
   */
  def translateXHorizontal(distance: Double, angle: Double): SimpleMatrix = {
    rotateY(angle).mult(translateX(distance).mult(rotateY(-angle)))
  }
  
   /**
   * Tekee z-suuntaisen translaatiomatriisin siten, että y-suuntaisen kierron vaikutus
   * poistetaan. Tämä luo vaikutelman, että liikutaan kohtisuoraan maanpintaan
   * nähden riippumatta kameran kulmasta.
   * @param angle on kameralta saatava kiertokulma
   */
  def translateZVertical(distance: Double, angle: Double): SimpleMatrix = {
    rotateY(angle).mult(translateZ(distance).mult(rotateY(-angle)))
  }
  
  /**
   * Tekee z-suuntaisen kiertomatriisin siten, että y-suuntaisen kierron vaikutus poistetaan.
   * Lopputuloksena kierto tapahtuu maanpinnan normaalin ympäri.
   * @param angle on kulma, joka kuvaa tehtävän kierron suuruutta
   * @param cAngle on kameralta saatava kiertokulma
   */
  def rotateZHorizontal(angle: Double, cAngle: Double) = {
    rotateY(cAngle).mult(rotateZ(angle).mult(rotateY(-cAngle)))
  }
  
  val smallRotLeft = this.rotateZ(Pi/20)
  val smallRotRight = this.rotateZ(-Pi/20)
  
}