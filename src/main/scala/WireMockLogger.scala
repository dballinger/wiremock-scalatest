import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.matching.{ValuePattern, RequestPattern}
import org.scalatest.{Failed, Suite}

trait WireMockLogger {
  this: Suite =>

  import collection.JavaConversions._

  def wiremockFailureLog = System.out

  def mockServers: Iterable[MockServer]

  override protected def withFixture(test: NoArgTest) = {
    test() match {
      case outcome: Failed =>
        printExpectedAndActualRequests(test)
        outcome
      case outcome =>
        outcome
    }
  }

  private def printExpectedAndActualRequests(test: NoArgTest) = {
    wiremockFailureLog.println(s"Wiremock expected and actual requests for failing test '${test.name}'")
    mockServers foreach printServerStubsAndRequests
  }

  private def printServerStubsAndRequests(ms: MockServer): Unit = {
    val server = ms.server
    printStubs(ms, server)
    wiremockFailureLog.println()
    printRequests(server)
  }

  private def printRequests(server: WireMockServer): Unit = {
    wiremockFailureLog.println("ACTUAL REQUESTS")
    server.findRequestsMatching(RequestPattern.everything()).getRequests.foreach {
      request =>
        val method = request.getMethod.value()
        val url = request.getUrl
        wiremockFailureLog.println(s"$method: $url")
        wiremockFailureLog.println("Headers:")
        request.getHeaders.all().foreach {
          header =>
            wiremockFailureLog.println(s"\t${header.key()}: ${header.values().mkString(",")}")
        }
        wiremockFailureLog.println("Body:")
        wiremockFailureLog.println(request.getBodyAsString)
    }
  }

  private def printStubs(ms: MockServer, server: WireMockServer): Unit = {
    val mappings = server.listAllStubMappings().getMappings
    val serverLine = s"WIREMOCK SERVER '${ms.name}'"
    wiremockFailureLog.println(serverLine)
    wiremockFailureLog.println(List.fill(serverLine.length)("=").mkString)
    if (mappings.isEmpty)
      wiremockFailureLog.println(s"DID NOT EXPECT ANY REQUESTS. DID YOU FORGET TO CONFIGURE ANY STUBS?")
    else
      wiremockFailureLog.println(s"STUB MAPPINGS:")
    mappings foreach {
      mapping =>
        val pattern = mapping.getRequest
        pattern match {
          case UrlMatcher(url) => wiremockFailureLog.println(s"${pattern.getMethod.value()}: $url")
          case _ => throw new scala.UnsupportedOperationException
        }
        wiremockFailureLog.println("With headers:")
        for {
          headers <- Option(pattern.getHeaders)
          header <- headers
        } {
          wiremockFailureLog.println(s"\t'${header._1}' ${ValueMatcher(header._2)}")
        }
        wiremockFailureLog.println("With body:")
        for {
          bodyPatterns <- Option(pattern.getBodyPatterns)
          bodyPattern <- bodyPatterns
        } {
          wiremockFailureLog.println(s"\t${ValueMatcher(bodyPattern)}")
        }
    }
  }

}

case class MockServer(name: String, server: WireMockServer)

class UrlMatcher(urlOrPath: String, policy: String, url: String) {
  override def toString = s"$urlOrPath $policy $url"
}

object UrlMatcher {
  def unapply(req: RequestPattern): Option[UrlMatcher] = (req.getUrl, req.getUrlPath, req.getUrlPattern, req.getUrlPathPattern) match {
    case (url, null, null, null) => Option(new UrlMatcher("URL", "==", url))
    case (null, path, null, null) => Option(new UrlMatcher("Path", "==", path))
    case (null, null, urlPattern, null) => Option(new UrlMatcher("URL", "~=", urlPattern))
    case (null, null, null, pathPattern) => Option(new UrlMatcher("Path", "~=", pathPattern))
    case _ => None
  }
}

class ValueMatcher(policy: String, value: String) {
  override def toString = s"$policy '$value'"
}

object ValueMatcher {
  def apply(vp: ValuePattern): ValueMatcher = {
    val allMatchers = List(
      ("equals", vp.getEqualTo),
      ("contains", vp.getContains),
      ("does not match", vp.getDoesNotMatch),
      ("equals json", vp.getEqualToJson),
      ("equals xml", vp.getEqualToXml),
      ("matches", vp.getMatches),
      ("matches json path", vp.getMatchesJsonPath),
      ("matches xpath", vp.getMatchesXPath)
    )
    allMatchers.find(_._2 != null) match {
      case Some((policy, value)) => new ValueMatcher(policy, value)
      case _ => throw new UnsupportedOperationException(s"Unsupported value pattern: $vp")
    }
  }
}