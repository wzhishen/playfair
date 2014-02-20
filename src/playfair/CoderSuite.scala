package playfair

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
 * Test suite for class Playfair.
 * 
 * This test suite is configured to be run within 
 * the Eclipse IDE based on JUnit 4 Runner. Comment
 * out necessary part if you need to run them directly 
 * on the command line.
 * 
 * @author Zhishen Wen
 * @version Nov 10, 2013
 * CIS 554
 */
@RunWith(classOf[JUnitRunner])
class CoderSuite extends FunSuite {
  
  test("Given a valid keyword, the code grid should be generated properly.") {
    val c = new Coder("Pennsylvania")
    val exp = "pensylvaibcdfghkmoqrtuwxz"
    val res = c.generateGrid
    assert(res == exp)
  }
  
  test("As for the digraphs, given text with double letters in a pair, an 'x' should be inserted between them.") {
    val c = new Coder("key")
    val input = "telling"
    val exp = List("te", "lx", "li", "ng")
    val res = c.generateDigraphs(input.toList)
    assert(res == exp)
  }
  
  test("As for the digraphs, given text without double letters in a pair, an 'x' should NOT be inserted between them.") {
    val c = new Coder("key")
    val input = "oppose"
    val exp = List("op", "po", "se")
    val res = c.generateDigraphs(input.toList)
    assert(res == exp)
  }
  
  test("As for the digraphs, given text with double letters 'xx' in a pair, an 'q' should be inserted between them.") {
    val c = new Coder("key")
    val input = "xxx"
    val exp = List("xq", "xq", "xz")
    val res = c.generateDigraphs(input.toList)
    assert(res == exp)
  }
  
  test("As for the digraphs, given text with odd length, a 'z' should be inserted in the end.") {
    val c = new Coder("key")
    val input = "power"
    val exp = List("po", "we", "rz")
    val res = c.generateDigraphs(input.toList)
    assert(res == exp)
  }
  
  test("Test whether 'encode' works properly.") {
    val c = new Coder("Pennsylvania")
    val input = "An anonymous reader sends word of a proof-of-concept Google Chrome browser extension that steals users' login details. The developer, Andreas Grech, says that he is trying to raise awareness about security among end users, and therefore chose Chrome as a test-bed because of its reputation as the safest browser."
    val exp = "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\ndckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\ndymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\nbonsn yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\ndckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\nkqxny m"
    val res = c.encode(input)
    assert(res == exp)
  }
  
  test("Test whether 'decode' works properly.") {
    val c = new Coder("Pennsylvania")
    val input = "fafaw aermw yqnvm vqyns genwm hwoln kqwow ofkpf nexcq wqfvp\ndckqu vhzwn ynmyz unsig wazcl wpxnv ipxey mpiqf asmvw lbvpx\ndymvd vaken obefm yinhq pdgyb npxfb zcsvp xzbas cxqki bynfn\nbonsn yniar wuynd tqbzp vowad sefxe ymnie fzcym ndqkp dfryn\ndckqu vinlw nyzlv mvyfl xenmg axpmy etwlx lwain zcnyf onyzl\nkqxny m"
    val exp = "anano nymou sread ersen dswor dofap roofo fconc eptgo xogle\nchrom ebrow serex tensi ontha tstea lsuse rslog indet ailst\nhedev elope randr easgr echsa ystha theis tryin gtora iseaw\narene ssabo utsec urity among endus ersan dther efore chose\nchrom easat estbe dbeca useof itsre putat ionas thesa festb\nrowse r"
    val res = c.decode(input)
    assert(res == exp)
  }
  
  test("Normalize a String: should only retain letters and convert them to lowercase.") {
    val c = new Coder("key")
    val input = "An anonymous reader sends word of a proof-of-concept Google Chrome browser extension that steals users' login details. The developer, Andreas Grech, says that he is trying to raise awareness about security among end users, and therefore chose Chrome as a test-bed because of its reputation as the safest browser."
    val exp = "ananonymousreadersendswordofaproofofconceptgooglechromebrowserextensionthatstealsuserslogindetailsthedeveloperandreasgrechsaysthatheistryingtoraiseawarenessaboutsecurityamongendusersandthereforechosechromeasatestbedbecauseofitsreputationasthesafestbrowser"
    val res = c.normalize(input)
    assert(res == exp)
  }
  
}