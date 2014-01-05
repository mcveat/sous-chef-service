package services

import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.trees.{Tree, TreePrint, PennTreebankLanguagePack}
import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}
import java.io.StringReader
import scala.collection.JavaConverters._
import scala.io.Source
import edu.stanford.nlp.ling.Word

/**
 * User: mcveat
 */
object Parser {
  val lexicalParser = LexicalizedParser loadModel "englishPCFG.ser.gz"
  val grammarStructureFactory = new PennTreebankLanguagePack().grammaticalStructureFactory()
  val tokenFactory = PTBTokenizer.factory(new CoreLabelTokenFactory(), "")

  def parse(text: String) = {
    val steps = Source.fromString(text).getLines().map(parseLine).flatten
    steps.map(treeToText).toList
  }

  private def parseLine(line: String) = {
    val tokens = tokenFactory.getTokenizer(new StringReader(line)).tokenize()
    val parsed = lexicalParser.apply(tokens)
//    new TreePrint("penn,typedDependenciesCollapsed").printTree(parsed)
    parsed.children().toList.map(_.children()).flatten.filterNot(_.value() == ",")
  }

  private def treeToText(t: Tree) = t.yieldWords().asScala.toList.map(wordValueToText).reduceLeft(_ + " " + _)

  private def wordValueToText(word: Word) = word.value() match {
    case "-LRB-" => "("
    case "-RRB-" => ")"
    case s => s
  }
}
