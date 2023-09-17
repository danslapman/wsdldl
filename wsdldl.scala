//> using scala 3.3.0
//> using dep org.scala-lang.modules::scala-xml:2.2.0
//> using dep com.softwaremill.sttp.client3::core:3.9.0

import java.nio.file.{Files, Paths, StandardOpenOption => SOO}

import scala.util.chaining._

import sttp.client3._
import sttp.model.Uri

import scala.xml._
import scala.xml.transform.RewriteRule

val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()

val inclLabels = Set("include", "import")

def printer = new scala.xml.PrettyPrinter(256, 4, true)

def fetch(uri: Uri) = backend.send(basicRequest.get(uri)).body.toOption

def extractLocations(elem: Node): Seq[String] = {
  elem.foldLeft(Seq.empty[String]) { (urls, node) =>
    node match {
      case Elem(_, label, attrs, _, _*) if inclLabels.contains(label) =>
        val attrMap = attrs.asAttrMap
        attrMap.get("schemaLocation").toSeq ++ attrMap.get("location").toSeq ++ urls
      case Elem(_, _, _, _, children @ _*) =>
        urls ++ children.flatMap(extractLocations)
      case _ => urls
    }
  }
}

def mkFileName(uri: Uri): String = uri match {
  case xmlUri if xmlUri.paramsMap.contains("xsd") => 
    val param = xmlUri.paramsMap("xsd")
    if (param.endsWith(".xsd")) param else param + ".xsd"
  case xmlUri if xmlUri.paramsMap.contains("wsdl") => 
    val param = xmlUri.paramsMap("wsdl")
    if (param.endsWith(".wsdl")) param else param + ".wsdl"
  case _ => "root.wsdl"
}

val pathRewriter = new RewriteRule() {
  override def transform(n: Node): Seq[Node] = n match {
    case Elem(prefix, label, attrs, scope, children @ _*) if inclLabels.contains(label) =>
      val attrMap = attrs.asAttrMap
      val schemaLocation = attrMap.get("schemaLocation")
        .map(sl => new UnprefixedAttribute("schemaLocation", mkFileName(uri"$sl"), Null))
      val location = attrMap.get("location")
        .map(sl => new UnprefixedAttribute("location", mkFileName(uri"$sl"), Null))
      val updated = attrs
        .pipe(ats => schemaLocation.fold(ats)(ats.append(_)))  
        .pipe(ats => location.fold(ats)(ats.append(_)))
      Seq(Elem.apply(prefix, label, updated, scope, true, transform(children): _*))
    case Elem(prefix, label, attrs, scope, children @ _*) =>
      Seq(Elem.apply(prefix, label, attrs, scope, true, transform(children): _*))
    case other => other
  }
}

def saveAndProcess(directory: String)(uri: Uri): Unit = {
  fetch(uri).foreach { receivedData =>
    val xmlData: Node = XML.loadString(receivedData)
    val path = Paths.get(directory + "/" + mkFileName(uri))

    if (!Files.exists(path))
      Files.write(
        path,
        printer.format(pathRewriter(xmlData)).getBytes("UTF-8"),
        SOO.CREATE,
        SOO.WRITE
      )

    extractLocations(xmlData)
      .map(s => uri"$s".host(uri.host.get).port(uri.port))
      .foreach(saveAndProcess(directory))
  }
}

@main
def main(url: String, output: String) = {
  saveAndProcess(output)(uri"$url")
}