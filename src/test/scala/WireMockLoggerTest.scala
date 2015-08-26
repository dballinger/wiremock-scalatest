import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.Charset

import com.github.tomakehurst.wiremock.WireMockServer
import com.google.common.base.Charsets
import org.scalatest._

import scala.collection.immutable.IndexedSeq
import scala.io.Source

class WireMockLoggerTest extends WordSpec with Matchers with BeforeAndAfterEach {

  import com.github.tomakehurst.wiremock.client.WireMock._

  trait Fixture extends FunSpecLike with WireMockLogger {
    var mockServer = new WireMockServer(0)
    mockServer.start()
    val mockServers = mockServer :: Nil

    val logCs: Charset = Charsets.UTF_8
    val out = new ByteArrayOutputStream()
    override val wiremockFailureLog: PrintStream = new PrintStream(out, true, logCs.name())

    val failedTest = Failed(new Exception)

    def log = Source.fromBytes(out.toByteArray, logCs.name()).getLines().toList
  }

  "" in new Fixture {
    mockServer.stubFor(get(urlEqualTo("/path")).willReturn(aResponse().withStatus(200)))
    withFixture(new NoArgTest {
      override def apply(): Outcome = ???

      override val text: String = _
      override val configMap: ConfigMap = _
      override val scopes: IndexedSeq[String] = _
      override val name: String = _
      override val tags: Set[String] = _
    })
//    withFixture(new Failed(new Exception))
    mockServer.stop()
  }
}