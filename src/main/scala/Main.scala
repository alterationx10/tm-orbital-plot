import java.io.{File, PrintWriter}

import scala.io.Source

case class Orbital(fileLines: Seq[String]) {

  private val nHeaderLines: Int = 11
  private val isoLimit: Double = 0.01

  val grid1: Grid = Grid(fileLines(4))
  val grid2: Grid = Grid(fileLines(5))
  val grid3: Grid = Grid(fileLines(6))

  private val nBlocks = grid2.nPoints * grid3.nPoints

  val data: Seq[GridData] = {
    val allData= for {
      index <- 0 until nBlocks
    } yield {
      val skip = nHeaderLines + (grid1.nPoints * index) + index
      fileLines.slice(skip, skip + grid1.nPoints).map(GridData)
    }
    allData.flatten.filter(p=> Math.abs(p.f) >= isoLimit)
  }

  val posDensity = {

    val xyPlane = data.filter(_.f > 0)
      .groupBy(d => (d.x, d.y))

    val outerPoints = xyPlane.flatMap{ d =>
      val sorted = d._2.sortBy(_.z)
      val maxZ = sorted.head
      val minZ = sorted.last
      Set(maxZ, minZ)
    }

    outerPoints.toSeq

  }

  val negDensity = {

    val xyPlane = data.filter(_.f < 0)
      .groupBy(d => (d.x, d.y))

    val outerPoints = xyPlane.flatMap{ d =>
      val sorted = d._2.sortBy(_.z)
      val maxZ = sorted.head
      val minZ = sorted.last
      Set(maxZ, minZ)
    }

    outerPoints.toSeq

  }
}

case class Grid(line: String) {
  private val data = line.replaceAll(" +", " ").split(" ")
  val start: Double = data(2).toDouble
  val delta: Double = data(4).toDouble
  val nPoints: Int = data(6).toInt

}

case class GridData(line: String) {
  private val data = line.replaceAll(" +", " ").split(" ").filter(!_.equals(""))
  val x: Double = data(0).toDouble
  val y: Double = data(1).toDouble
  val z: Double = data(2).toDouble
  val f: Double = data(3).toDouble
}

object Main extends App {

  val fileName = "1e2u2.xyz"
  val fileLines = Source.fromFile(fileName).getLines().toSeq
  val orbital: Orbital = Orbital(fileLines)

  val posPw = new PrintWriter(new File("pos.xyz"))
  val negPw = new PrintWriter(new File("neg.xyz"))

  orbital.posDensity.foreach{ d =>
    posPw.println(s"${d.x} ${d.y} ${d.z} ${d.f}")
  }
  posPw.flush()
  posPw.close()

  orbital.negDensity.foreach{ d =>
    negPw.println(s"${d.x} ${d.y} ${d.z} ${d.f}")
  }
  negPw.flush()
  negPw.close()
}
