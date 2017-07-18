import org.jsoup.Jsoup
import org.jsoup.select.Elements
import org.jsoup.nodes.{Document, Element, Attributes}
import java.io._

case class Ticker(title:String, stages:String,indication:String, price:Double, name:String = "")

object Oncobrowser extends App {

    var minPrice = 1.0
    var maxPrice = 100.0
    var cancers:List[String] = List("all")

    print("Min Price: ")
    minPrice = try {
            readLine().toDouble
        } catch{
            case e:Exception =>
            1.0
        }

    print("Max Price: ")
    maxPrice = try {
            readLine().toDouble
        } catch{
            case e:Exception =>
            100.0
        }

    print("Cancers: ")
    cancers = try {
            val s = readLine()
            val arr = s.split(",").filter{ x=> x.trim.size > 0 }
            if (arr.size > 0) {
                arr.toList
            } else List("all")

        } catch{
            case e:Exception =>
            List("all")
        }

    printf("Settings:%s, $%.2f to $%.2f\n", cancers.mkString(","), minPrice, maxPrice)

    val url = "https://biopharmcatalyst.com/companies/company-pipeline-database#marketCap=microunder|sort=company"

    getTickers(mkSrc)
    .filter{ t:Ticker => t.price >= minPrice && t.price <= maxPrice}
    .filter{ t:Ticker => 
        if (cancers.head == "all") true
        else cancers.map{ c:String => t.indication.contains(c) }.reduceLeft(_ && _)
    }
    .sortBy{ x=> x.price}
    .zipWithIndex.foreach{ case (t:Ticker, i:Int) =>
        printf("%d.%s\t%.2f\t%s\n", i, t.name, t.price, "https://www.google.com/search?query="+t.name)
        println(t.title)
        println(t.stages)
        println(t.indication)
        println("\n\n")
    }


    def getTickers(f:File):List[Ticker] = {
        val doc:Document = Jsoup.parse(f,"UTF-8")
        val drugs = doc.getElementsByClass("drugList drugIndications fda stages stage-labels")
        val comps = doc.getElementsByClass("company-details__list")
        val names = doc.getElementsByClass("company__info")
        assert(drugs.size == comps.size)
        assert(drugs.size == names.size)

        var tickers:List[Ticker] = List.empty

        (0 until drugs.size).foreach{ i=> 
            val attr:Attributes = drugs.get(i).getAllElements().get(0).attributes()
            val indication = attr.get("data-drug-indications").toLowerCase
            val title = attr.get("data-drug-titles")
            val stages = attr.get("data-stage-labels")
            val price = comps.get(i).getAllElements().get(2).attr("data-value").toDouble
            val name = names.get(i).getAllElements().get(1).attributes().get("data-value")
            
           if (indication.contains("cancer") || indication.contains("tumor")) {
                val ticker = Ticker(title,stages,indication,price, name)
                tickers = ticker::tickers
            }
        }
        tickers
    }

    def mkSrc:File = {
        import sys.process._
        val cmd = "wget -q -O file.html " + url
        cmd.!!
        new File("./file.html")  
    }
}
