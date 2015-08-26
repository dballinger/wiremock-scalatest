import com.github.tomakehurst.wiremock.WireMockServer
import org.scalatest.{Failed, Suite}

trait WireMockLogger {
  this: Suite =>

  def wiremockFailureLog = System.out

  def mockServers:Iterable[MockServer]

  override protected def withFixture(test: NoArgTest) = {
    test() match {
      case outcome: Failed =>
        wiremockFailureLog.println(s"Ooops... ${test.name}")
        outcome
      case outcome =>
        outcome
    }
  }

}

case class MockServer(name:String, server:WireMockServer)