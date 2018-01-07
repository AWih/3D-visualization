package mallinnus
import org.ejml.simple._

/**
 * Seinä on ohjelman keskeinen luokka, joka kuvaa laskettavia ja piirrettäviä
 * suorakulmia.
 * Seinät voisivat olla periaatteessa mielivaltaisen muotoisia 2D-muotoja, mutta
 * seinien leikkaaminen ja törmäyspisteiden laskeminen vaativat, että kulmia on
 * 4 kpl ja törmäyspisteiden laskeminen olettaa lisäksi että seinä on suorakulmion
 * muotoinen.
 * @param coord Sisältää seinän koordinaatit Vectorissa SimpleMatrix-tyyppisinä.
 * Käytettävät koordinaatit ovat neliulotteisia, sillä ohjelma käyttää homogeenisia
 * koordinaatteja. Koordinaatteja täytyy olla neljä kappaletta ja niiden tulee
 * olla keskenään samassa tasossa. Lisäksi koordinaattien tulee muodostaa suorakulmio.
 */
class Wall(coord: Vector[SimpleMatrix]) {
  require(coord.length == 4)
  
  /**
   * Alkuperäiset koordinaatit, jotka otetaan parametrina.
   * Nämä säilytetään, koska ohjelman toimintaperiaate on muuttaa
   * ympäristön sijaintia kameran sijainnin sijasta, jolloin seinien koordinaatit
   * muuttuvat jatkuvasti.
   * Alue voidaan palauttaa alkutilaan näiden koordinaattien avulla.
   */
  val coordinates: Vector[SimpleMatrix] = coord
  
  /**
   * Suhteelliset koordinaatit ovat seinän koordinaatit, joita muutetaan
   * liikkumisen yhteydessä. Nämä ovat koordinaatit "suhteessa" kameraan.
   */
  var relativeCoords: Vector[SimpleMatrix] = this.coordinates
  
  /**
   * Seuraavat koordinaatit
   */
  var nextCoords: Vector[SimpleMatrix] = this.coordinates
    
  /**
   * Laskee seinälle pisteitä, joilla seinä täytetään. Näiden avulla
   * määritetään käyttäjän etäisyys seinästä.
   * Käytännössä vaatii, että seinä on suorakulman muotoinen
   * ja yhdessä tasossa.
   */
  def getCollisionPoints: Vector[SimpleMatrix] = {
    val vertex = relativeCoords(0)
    val a = relativeCoords(1).minus(relativeCoords(0))
    val b = relativeCoords(3).minus(relativeCoords(0))
    val au = a.divide(a.normF())
    val bu = b.divide(b.normF())
    var points: Array[SimpleMatrix] = Array()
    for (i <- 0 to a.normF().round.toInt by 1) {
      for (j <- 0 to b.normF().round.toInt by 1) {
        val newPoint: SimpleMatrix = vertex.plus(au.scale(i).plus(bu.scale(j)))
        points = points ++ Array[SimpleMatrix](newPoint)
      }
    }
    points.toVector
  }
  
  var collisionPoints: Vector[SimpleMatrix] = this.getCollisionPoints
                                                                      
  var nextCollisionPoints: Vector[SimpleMatrix] = this.collisionPoints
                                 
  /**
   * Määrittää törmäyksen seinään: palauttaa true jos seinään ollaan törmäämässä
   * ja muuten false.
   * Törmäys tapahtuu, jos seinä on riittävän lähellä ja jos se tulisi vielä lähemmäksi.
   */
  def checkCollision: Boolean = {
    val d = this.getDistance
    val dn = this.getNextDistance
    (d < 1.5) && (dn < d)
  }
  
  /**
   * Seinän kaksiulotteiset koordinaatit piirtämistä varten.
   */
  var twoDimCoords: Option[Vector[(Double, Double)]] = None
  
  /**
   * Kaksiulotteiset törmäyspisteet piirtämistä varten.
   */
  var twoDimColPoints: Option[Vector[(Double, Double)]] = None
    
  /**
   * Laskee etäisyyden kamerasta seinän lähimmän törmäyspisteen mukaan.
   */
  def getDistance: Double = this.collisionPoints.map { x => x.normF() }.min
  
  /**
   * Laskee etäisyyden seinän kauimmaisen törmäyspisteen mukaan.
   */
  def getMaxDistance: Double = this.collisionPoints.map { x => x.normF() }.max
  
  /**
   * Seuraava pienin etäisyys kamerasta, jos pyydetty muunnos toteutetaan.
   */
  def getNextDistance: Double = this.nextCollisionPoints.map { x => x.normF() }.min 
  
  /**
   * Laskee liikkumisen tuloksen valmiiksi matriisikertolaskun avulla, mutta ei
   * vielä toteuta muunnosta.
   */
  def prepareTransform(m: SimpleMatrix) = {
    this.nextCoords = this.nextCoords.map { x => m.mult(x) }
    this.nextCollisionPoints = this.nextCollisionPoints.map {x => m.mult(x)}
  }
  
  /**
   * Siirtää muuttuneen sijainnin varsinaisiin käytettäviin koordinaatteihin.
   */
  def applyTransform() = {
    this.relativeCoords = this.nextCoords
    this.collisionPoints = this.nextCollisionPoints
  }
  
  /**
   * Palauttaa seuraavat koordinaatit ennalleen.
   */
  def cancelTransform() = {
    this.nextCoords = this.relativeCoords
    this.nextCollisionPoints = this.collisionPoints
  }
  
  /**
   * Pyytää projektiotasolta uudet 2D-koordinaatit
   */
  def updateTwoDimCoords(s: ProjectionSurface) = this.twoDimCoords = s.getWallProjection(this)
  
  /**
   * Pyytää projektiotasolta uudet 2D-koordinaatit törmäyspisteille
   */
  def updateTwoDimColPoint(s: ProjectionSurface) = this.twoDimColPoints = s.getColPointProjection(this)
  
  /**
   * Palauttaa seinään sen alkutilaan.
   */
  def reset() = {
    this.relativeCoords = this.coordinates
    this.nextCoords = this.coordinates
    this.collisionPoints = this.getCollisionPoints
    this.nextCollisionPoints = this.collisionPoints
  }
  
}