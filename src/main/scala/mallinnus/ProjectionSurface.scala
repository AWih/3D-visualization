package mallinnus
import org.ejml.simple._
import scala.collection.parallel._


/**
 * Projektiotaso tekee 2D-projektiot 3D (tai oikeastaan 4D) -koordinaateista, sekä
 * sisältää projektioihin tarvittavia apumetodeja.
 * Taso on yhdensuuntainen yz-tason kanssa ja sen x-koordinaatti on parametrina annattu xValue.
 */
class ProjectionSurface(xValue: Int) {
  require(xValue != 0)
  val x: Int = xValue
  
  /**
   * Tämä metodi laskee laskee annetun paikkavektorin leikkauspisteen tämän tason kanssa.
   * Paikkavektori v on oltava 4x1-kokoinen matriisi.
   */
  def getVectorIntersectionPoint(v: SimpleMatrix): Option[(Double, Double)] = {
    require(v.numRows() == 4 && v.numCols() == 1)
    val vectorX = v.get(0, 0)
    /*
     * Leikkauspiste lasketaan vain pisteille, joilla on positiivinen x-koordinaatti.
     * Seinien piirtäminen ei tarvitse tätä tarkistusta, sillä seinien leikkaaminen
     * tehdään tätä ennen. Sen sijaan törmäyspisteet (collision points) tarvitsevat
     * tämän tarkistuksen.
     */
    if (vectorX > 0) {
      val scaledVector: SimpleMatrix = v.divide(vectorX).scale(this.x)
      Some((scaledVector.get(1, 0), scaledVector.get(2, 0)))
    }
    else {
      None: Option[(Double, Double)]
    }
  }
  
  /**
   * cut on apumetodi seinien leikkaamista varten.
   * cut ottaa parametrina vektorin v päätepisteet ja palauttaa pisteen
   * v:n määräämältä suoralta siten, että piste on juuri x-akselin positiivisella
   * puolella.
   */
  private def cut(x1: SimpleMatrix, x2: SimpleMatrix): SimpleMatrix = {
    val v = x2.minus(x1)        //v = x2 - x1
    val u = v.divide(v.normF()) //u on v:n yksikkövektori
    var t = 0
    var cut = x1.plus(u.scale(t)) //cut = x1 + t*u,  t on skalaari
    var next = cut.copy()
    while (next.get(0, 0) > 0.1) {
      cut = next.copy
      t += 1
      next = x1.plus(u.scale(t))
    }
    cut
  }
  
  /**
   * getWallProjection on metodi, joka antaa seinälle sitä vastaavat 2-ulotteiset
   * koordinaatit piirtämistä varten.
   */
  def getWallProjection(w: Wall): Option[Vector[(Double, Double)]] = {
    //Koordinaatit myötäpäivään alkaen vasemmasta yläkulmasta.
    val a = w.relativeCoords(0)
    val b = w.relativeCoords(1)
    val c = w.relativeCoords(2)
    val d = w.relativeCoords(3)
    
    /**
     * Seinä "leikataan" siten, että seinän mikään kulma ei mene x-akselin negatiiviselle puolelle,
     * eli yz-tason "väärälle" puolelle. 
     * Tehtävä leikkaus riippuu siitä, mitkä koordinaateista ovat "vääriä" ja mitkä eivät.
     * Huom! Tapauksesta riippuen myös piirrettävien koordinaattien määrä voi muuttua. Esim.
     * jos vasen yläkulma (a) on "väärä" ja muut ovat oikeita, täytyy a korvata leikkauksella
     * b:stä a:han ja d:stä a:han, siis kahdella pisteellä.
     */
    val cutCoodinates = w.relativeCoords.map { x => (x.get(0, 0) > 0.1) } match {
      case Vector(true,  true,  true,  true ) => w.relativeCoords
      
      case Vector(false, true,  true,  true ) => Vector(cut(b, a), b,         c,         d,         cut(d, a))
      case Vector(true,  false, true,  true ) => Vector(a,         cut(a, b), cut(c, b), c,         d        )
      case Vector(true,  true,  false, true ) => Vector(a,         b,         cut(b, c), cut(d, c), d        )
      case Vector(true,  true,  true,  false) => Vector(a,         b,         c,         cut(c, d), cut(a, d))
      
      case Vector(false, false, true,  true ) => Vector(cut(d, a), cut(c, b), c,         d        )
      case Vector(true,  false, false, true ) => Vector(a,         cut(a, b), cut(d, c), d        )
      case Vector(true,  true,  false, false) => Vector(a,         b,         cut(b, c), cut(a, d))
      case Vector(false, true,  true,  false) => Vector(cut(b, a), b,         c,         cut(c, d))
      
      case Vector(true,  false, false, false) => Vector(a,         cut(a, b), cut(a, d))
      case Vector(false, true,  false, false) => Vector(cut(b, a), b,         cut(b, c))
      case Vector(false, false, true,  false) => Vector(cut(c, b), c,         cut(c, d))
      case Vector(false, false, false, true ) => Vector(cut(d, a), cut(d, c), d        )
      
      case Vector(false, false, false, false) => Vector()
      case _ =>                                  Vector() //ei pitäisi tapahtua
    }
    
    val intersections = cutCoodinates.map {this.getVectorIntersectionPoint(_)}
    if (!intersections.isEmpty) Some(intersections.filter(_.isDefined).map(_.get))
    else None

  }
  
  /**
   * Laskee vastaavat 2D-projektiot seinien törmäyspisteille.
   */
  def getColPointProjection(w: Wall): Option[Vector[(Double, Double)]] = {
    val intersections = w.collisionPoints.map {this.getVectorIntersectionPoint(_)}
    if (intersections.forall(_.isDefined)) {
      Some(intersections.map {x => x.get})
    }
    else if (intersections.exists(_.isDefined)) {
      val validIntersections = intersections.filter(_.isDefined)
      Some(validIntersections.map( {x => x.get}))
    }
    else None
  }
}