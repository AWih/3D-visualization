package mallinnus
import org.ejml.simple._
import java.io._
import scala.collection.mutable.Buffer


class World {
  
  /**
   * walls sisältää kaikki seinät, jotka ovat mukana visualisoinnissa
   */
  var walls: Vector[Wall] = Vector[Wall]()
  
  //projektiotason x-koordinaatiksi annetaan oletuksena 1, mutta valinta on mielivaltainen
  var surface: ProjectionSurface = new ProjectionSurface(1)
  
  /**
   * Lataa seinät tiedostosta.
   * Tiedosto on csv-tyyppinen, jossa koordinaattien komponentit on erotettu
   * toisistaan puolipisteellä.
   * Seinät ladataan "worlddata.csv"-nimisestä tiedostosta.
   */
  def loadWorld(): Unit = {
    var newWalls = Buffer[Wall]()
    
    val fileReader = try {
      new FileReader("worlddata.csv")
    }
    catch {
      case e: FileNotFoundException => {
        println("""tiedostoa "worlddata.csv" ei löydy""")
        return
      }
    }
    val lineReader = new BufferedReader(fileReader)
    
    try {
      var line = lineReader.readLine()
      
      var wallData = Buffer[SimpleMatrix]()
      while (line != null) {
        val strippedData = line.takeWhile { x => x != '#' }.trim()
        val txtData = strippedData.split(';')
        if (txtData.isEmpty) {                      //tyhjä rivi seinien datan välissä
          if (!wallData.isEmpty) {                  //jos on kerätty dataa, se laitetaan talteen
            newWalls += new Wall(wallData.toVector)
            wallData = Buffer[SimpleMatrix]()       //valmistellaan seuraavan seinän lukeminen
          }
        }
        else {                                      //tavallinen lukeminen
          val data: Array[Double] = txtData.map { x => x.toDouble }
          wallData += new SimpleMatrix(Array(data.:+(1.toDouble))).transpose()
        }
        line = lineReader.readLine()
      }
      if (!wallData.isEmpty) {
        newWalls += new Wall(wallData.toVector)
      }
      this.walls = newWalls.toVector.sortBy { w => -w.getDistance }
      lineReader.close()
      fileReader.close()
    }
    catch {
      case e: IOException => return
    }
  }
  
  def clearWorld() = this.walls = Vector[Wall]()
  
  val transforms = Transforms
  
  /**
   * applyTransform muuttaa maailmassa olevien esineiden paikkavektoreita kertomalla
   * ne jollakin matriisilla m. Tällä metodilla toteutetaan maailmassa liikkuminen: katselupiste
   * pysyy koko ajan samana ja ympärillä olevia esineitä siirretään.
   */
  def applyTransform(m: SimpleMatrix): Boolean = {
    var ret = false
    this.walls.par.foreach { w => w.prepareTransform(m) }
    //törmäys tarkistetaan ennen lopullista muunnosta
    if (!this.walls.exists { w => w.checkCollision }) {
      this.walls.par.foreach {w => w.applyTransform()}
      ret = true
    }
    else {
      this.walls.par.foreach { w => w.cancelTransform() }
    } 
    /*
     * Seinät lajitellaan kauimmasta lähimpään, jolloin lähempänä olevat seinät
     * piirtyvät taempien päälle.
     * Jos seinien pienimmät etäisyydet ovat samat (esim. kahden seinän kulma on
     * molemmista lähimpänä) käytetään vaihtoehtoisesti seinien kauimmaista etäisyyttä.
     */
    this.walls = this.walls.sortBy { w => (-w.getDistance, -w.getMaxDistance) }
    ret
  }
  
  /**
   * updateTwoDimCoords ottaa esineistä projektion projektiotasolle ja asettaa näin saadut
   * 2-ulotteiset koordinaatit esineen twoDimCoords-muuttujaan.
   */
  def updateTwoDimCoords() = walls.par.foreach { w => w.updateTwoDimCoords(this.surface) }
  
  def updateTwoDimColPoints() = walls.par.foreach { w => w.updateTwoDimColPoint(this.surface) }
  
  /**
   * Palauttaa maailman alkutilanteeseen.
   */
  def resetWorld() = {
    walls.foreach { w => w.reset() }
    this.walls = walls.sortBy { w => -w.getDistance }
  }
  
  
}