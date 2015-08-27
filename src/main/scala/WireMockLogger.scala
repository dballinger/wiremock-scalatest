import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.matching.{ValuePattern, RequestPattern}
import org.scalatest.{Failed, Suite}

trait WireMockLogger {
  this: Suite =>

  import collection.JavaConversions._

  def wiremockFailureLog = System.out

  def mockServers: Iterable[MockServer]

  def printExpectedAndActualRequests(test: NoArgTest) = {
    wiremockFailureLog.println(s"Wiremock expected and actual requests for failing test '${test.name}'")
    mockServers foreach {
      ms =>
        val server = ms.server
        val mappings = server.listAllStubMappings().getMappings
        if (mappings.isEmpty)
          wiremockFailureLog.println(s"Wiremock server '${ms.name}' did not expect any requests. Did you forget to configure any stubs?")
        else
          wiremockFailureLog.println(s"Wiremock server '${ms.name}' stub mappings:")
        mappings foreach {
          mapping =>
            val pattern = mapping.getRequest
            pattern match {
              case UrlMatcher(url) => wiremockFailureLog.println(s"${pattern.getMethod.value()}: $url")
              case _ => throw new UnsupportedOperationException
            }
            wiremockFailureLog.println("With headers:")
            for {
              headers <- Option(pattern.getHeaders)
              header <- headers
            } {
              wiremockFailureLog.println(s"\t'${header._1}' ${ValueMatcher(header._2)}")
            }
        }
    }
  }

  override protected def withFixture(test: NoArgTest) = {
    test() match {
      case outcome: Failed =>
        printExpectedAndActualRequests(test)
        outcome
      case outcome =>
        outcome
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
  def apply(vp: ValuePattern): ValueMatcher = (vp.getEqualTo, vp.getContains) match {
    case (eq, null) => new ValueMatcher("equals", eq)
    case (null, cont) => new ValueMatcher("contains", cont)
    case _ => throw new UnsupportedOperationException(s"Unsupported value pattern: $vp")
  }
}