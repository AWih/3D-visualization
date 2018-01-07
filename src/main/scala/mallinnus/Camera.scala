package mallinnus
import scala.math.{tan, Pi, max}
import org.ejml.simple._

/**
 * Kamera kuvaa visualisoinnin katselupistettä.
 * Luokka sisältää tietoa käytettävästä näkökentästä,
 * sekä hetkellisen korkeuden sekä katselukulman (y-akselin ympäri).
 */
class Camera(surface: ProjectionSurface, width: Int, height: Int) {
  /**
   * Camera-luokka sisältää näkökentän laajuuteen tarvittavaa tietoa.
   */
  
  private var angle: Double = 0  //kulma y-akselin ympäri
  
  def getAngle = this.angle
  
  /**
   * Muuttaa kameran kulmaa.
   */
  def rotate(a: Double) = this.angle = this.angle + a
  
  /**
   * Kameran "korkeus". Tällä voidaan varmistaa, että kamera
   * ei mene tietyn korkeuden alle.
   */
  private var cameraHeight: Double = 0
  
  def lift(d: Double) = this.cameraHeight = this.cameraHeight + d
  
  def lower(d: Double) = {
    this.cameraHeight = max(this.cameraHeight - d, 0)
  }
  
  def canLower(d: Double): Boolean = {
    this.cameraHeight - d > 0
  }
  
  /**
   * Näkökentän laajuus vaakasuunnassa
   */
  val horizontalFOV: Int = 100                                                   //degrees
  
  //pystysuora FOV lasketaan piirtoalueen mittasuhteiden perusteella
  val verticalFOV: Int = ((height.toDouble/width) * horizontalFOV).round.toInt   //degrees
  
  /**
   * y:n ja z:n maksimiarvot rajaavat projektiotason alueen, jolla olevat pisteet
   * piirretään. Alueen ulkopuolella oleva tieto jää piirtoalueen ulkopuolelle.
   */
  val ymax = surface.x * tan((this.horizontalFOV.toDouble / 2) * (Pi/180))
  val ymin = -ymax
  val zmax = surface.x * tan((this.verticalFOV.toDouble / 2) * (Pi/180))
  val zmin = -zmax
  
  def ylen = 2 * ymax
  def zlen = 2 * zmax
  
  /**
   * Reset nollaa kameran tilan.
   */
  def reset(): Unit = {
    angle = 0
    cameraHeight = 0
  }
}