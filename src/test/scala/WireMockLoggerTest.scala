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
    var mockServer1 = MockServer("server1", new WireMockServer(0))
    var mockServer2 = MockServer("server2", new WireMockServer(0))
    val mockServers = mockServer1 :: mockServer2 :: Nil
    mockServers foreach {_.server.start()}

    def stopStubbing() = mockServers foreach {_.server.stop()}

    val logCs: Charset = Charsets.UTF_8
    val out = new ByteArrayOutputStream()
    override val wiremockFailureLog: PrintStream = new PrintStream(out, true, logCs.name())

    val passingTest = new NoArgTest {
      override def apply(): Outcome = Succeeded

      override val text: String = "it passed"
      override val configMap: ConfigMap = new ConfigMap(Map.empty[String, Any])
      override val scopes: IndexedSeq[String] = IndexedSeq.empty[String]
      override val name: String = "pass"
      override val tags: Set[String] = Set.empty[String]
    }
    val failingTest = new NoArgTest {
      override def apply(): Outcome = Failed(new Exception)

      override val text: String = "it failed"
      override val configMap: ConfigMap = new ConfigMap(Map.empty[String, Any])
      override val scopes: IndexedSeq[String] = IndexedSeq.empty[String]
      override val name: String = "fail"
      override val tags: Set[String] = Set.empty[String]
    }

    def log = Source.fromBytes(out.toByteArray, logCs.name()).getLines().toList
  }

  "A passing test should not produce any stubbing log" in new Fixture {
    withFixture(passingTest)
    log shouldBe Nil
    stopStubbing()
  }

  "A failing test should print the name of the test" in new Fixture {
    withFixture(failingTest)
    log.head shouldBe s"Wiremock expected and actual requests for failing test '${failingTest.name}'"
    stopStubbing()
  }

  "A failing test using an unstubbed wiremock server should explain that there were no expected stubbings" in new Fixture {
    withFixture(failingTest)
    log should contain(s"Wiremock server '${mockServer1.name}' did not expect any requests. Did you forget to configure any stubs?")
    log should contain(s"Wiremock server '${mockServer2.name}' did not expect any requests. Did you forget to configure any stubs?")
    stopStubbing()
  }

  "A failing test using a wiremock server with multiple stubs should print the expected method and url matchers" in new Fixture {
    val getUrl = "/url1"
    val postPath = "/url2"
    val putUrlMatch = "/url3"
    val deletePathMatch = "/url4"

    mockServer1.server.stubFor(get(urlEqualTo(getUrl)).willReturn(aResponse().withStatus(200)))
    mockServer1.server.stubFor(post(urlPathEqualTo(postPath)).willReturn(aResponse().withStatus(200)))
    mockServer1.server.stubFor(put(urlMatching(putUrlMatch)).willReturn(aResponse().withStatus(200)))
    mockServer1.server.stubFor(delete(urlPathMatching(deletePathMatch)).willReturn(aResponse().withStatus(200)))
    withFixture(failingTest)

    val server1Log = log.takeWhile {!_.contains(mockServer2.name)}

    server1Log should contain(s"Wiremock server '${mockServer1.name}' stub mappings:")
    server1Log should contain(s"GET: URL == $getUrl")
    server1Log should contain(s"POST: Path == $postPath")
    server1Log should contain(s"PUT: URL ~= $putUrlMatch")
    server1Log should contain(s"DELETE: Path ~= $deletePathMatch")

    stopStubbing()
  }

  "A failing test using a wiremock server with header stubs should print the expected headers" in new Fixture {
    val equalHeaderName = "a"
    val equalHeaderValue = "1"
    val containingHeaderName = "b"
    val containingHeaderValue = "2"
    val url = "/url"
    mockServer1.server.stubFor(get(urlEqualTo(url))
        .withHeader(equalHeaderName, equalTo(equalHeaderValue))
        .withHeader(containingHeaderName, containing(containingHeaderValue))
        .willReturn(aResponse().withStatus(200))
    )
    withFixture(failingTest)
    val headerLog = log.dropWhile {!_.contains(url)}.tail.take(3)

    headerLog.head shouldBe "With headers:"
    headerLog.tail should contain(s"\t'$equalHeaderName' equals '$equalHeaderValue'")
    headerLog.tail should contain(s"\t'$containingHeaderName' contains '$containingHeaderValue'")

    stopStubbing()
  }
}