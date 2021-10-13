// Copyright (C) 2017, 2021 John Maraist
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa.samples
import scala.language.adhocExtensions
import java.io.File
import scala.collection.mutable.Builder
import org.maraist.util.FilesCleaner
import org.maraist.latex.{LaTeXdoc, Sampler}
import org.maraist.fa.{
  DFA, NFA, DFABuilder, NFABuilder
  // , PFA, PFABuilder
  // , HyperedgeDFA, HyperedgeNFA, HyperedgeDFABuilder, HyperedgeNFABuilder
  // , EdgeAnnotatedDFA, EdgeAnnotatedNFA,
  // EdgeAnnotatedDFABuilder, EdgeAnnotatedNFABuilder
}
import org.maraist.fa.elements.{
  AddState, RemoveState, RemoveFinalState, AddFinalState,
  RemoveTransition, AddTransition,
  DFAelements, NFAelements
}

/**
 * Sample automata, and printing a guide to them.
 */
object Samples extends Sampler {

  /**
   * Return a fresh copy of a sample DFA builder
   */
  def dfa1B = {
    val builder = DFA.newBuilder[String,Int]("A")
    builder += AddState[String,Int]("B")
    builder += AddState("C")
    builder += AddFinalState("D")
    builder += AddTransition("A", 1, "B")
    builder += AddTransition("A", 2, "C")
    builder += AddTransition("B", 3, "D")

    // val builder = new DFABuilder[String,Int]("A")
    // builder.addState("B")
    // builder.addState("C")
    // builder.addFinalState("D")
    // builder.addTransition("A", 1, "B")
    // builder.addTransition("A", 2, "C")
    // builder.addTransition("B", 3, "D")

    builder
  }

  /**
   * Return a DFA derived from a fresh copy of the builder
   * {@link dfa1B}
   */
  def dfa1: DFA[String,Int] = {
    dfa1B.result()
  }

  /**
   * Return a fresh copy of a sample NFA builder
   */
  def ndfa2B = {
    val builder = new NFABuilder[String,Int]()
    builder.addInitialState("A")
    builder.addState("B")
    builder.addState("C")
    builder.addFinalState("D")
    builder.addTransition("A", 1, "B")
    builder.addTransition("A", 2, "C")
    builder.addTransition("B", 3, "D")
    builder.addETransition("C", "A")
    builder
  }

  /**
   * Return an NFA, derived from a fresh copy of the sample builder
   * {@link ndfa2B}
   */
  def ndfa2: NFA[String,Int] = {
    ndfa2B.result()
  }

  /**
   * Return a DFA converted from a fresh copy of the sample NFA builder
   * {@link ndfa2B}
   */
  def ndfa2dfa: DFA[Set[String],Int] = {
    ndfa2.toDFA
  }

  /**
   * Return a fresh copy of a sample DFA-with-hyperedge builder
  def hdfa3B: HyperedgeDFABuilder[
    String, Int, HyperedgeDFA[String,Int],
    HyperedgeDFAelements[String,Int]
  ] = {
    val builder = new HyperedgeDFABuilder[String,Int]("A")
    builder.addState("B")
    builder.addState("C")
    builder.addFinalState("D")
    builder.addTransition("A", 1, "B")
    builder.addTransition("A", 2, "C")
    builder.addTransition("B", 3, "D")
    builder.addEHyperedge("C", Set[String]("B", "D"))
    builder
  }
   */

  /**
   * Return a DFA converted from a fresh copy of the sample
   * DFA-with-hyperedge builder {@link hdfa3B}
  def hdfa3: HyperedgeDFA[String,Int] = {
    val res: HyperedgeDFA[String,Int] = hdfa3B.result()
    res
  }
   */

  /**
   * Return a fresh copy of a sample NFA-with-hyperedge builder
  def hndfa4B: HyperedgeNFABuilder[String, Int, HyperedgeDFA[Set[String], Int], HyperedgeNFA[String,Int, HyperedgeDFA[Set[String], Int]]] = {
    val builder = new HyperedgeNFABuilder[String,Int]
    builder.addInitialState("Z")
    builder.addState("A")
    builder.addState("B")
    builder.addState("C")
    builder.addFinalState("D")
    builder.addState("E")
    builder.addState("F")
    builder.addState("G")
    builder.addState("H")
    builder.addState("I")
    builder.addState("J")
    builder.addState("K")
    builder.addState("L")
    builder.addTransition("Z", 7, "A")
    builder.addTransition("Z", 8, "C")
    builder.addTransition("Z", 9, "E")
    builder.addTransition("A", 1, "B")
    builder.addTransition("C", 2, "D")
    builder.addTransition("E", 3, "F")
    builder.addTransition("G", 4, "J")
    builder.addTransition("H", 5, "K")
    builder.addTransition("I", 6, "L")
    builder.addETransition("A", "C")
    builder.addETransition("C", "E")
    builder.addETransition("H", "I")
    builder.addEHyperedge("C", Set[String]("G", "H"))
    builder
  }
   */

  /**
   * Return a NFA converted from a fresh copy of the sample
   * NFA-with-hyperedge builder {@link hndfa4B}
  def hndfa4: HyperedgeNFA[String, Int, HyperedgeDFA[Set[String], Int]] = {
    hndfa4B.result()
  }
   */

  /**
   * Return a DFA converted from a fresh copy of the sample
   * NFA-with-hyperedge builder {@link hndfa4B}
  def hndfa4dfa: HyperedgeDFA[Set[String],Int] = {
    hndfa4.toDFA
  }
   */

  /**
  def dlhPfa57:PFABuilder[Int,String] = {
    val res = new PFABuilder[Int,String]
    res.addInitialState(1, 1.0)
    res.addFinalState(2, 0.4)
    res.addFinalState(3, 0.2)
    res.addFinalState(4, 0.6)

    res.addTransition(1,"a",3, 0.5)
    res.addETransition(1,2, 0.5)

    res.addETransition(2,2, 0.5)
    res.addTransition(2,"a",4, 0.1)

    res.addETransition(3,2, 0.2)
    res.addETransition(3,3, 0.5)
    res.addTransition(3,"a",4, 0.1)

    res.addETransition(4,3, 0.2)
    res.addTransition(4,"a",4, 0.2)

    res
   }
   */

  /**
  def dlhPfa57_erem: PFABuilder[Int,String] = {
    val res = dlhPfa57
    res.removeEpsilonTransitions
    res
  }
   */

  /**
  def ann01_builder: NFAEdgeAnnotationsBuilder[
    String, Char, Int, Set[Int],
    ? <: DFA[Set[String],Char] & EdgeAnnotatedDFA[Set[String],Char,Set[Int]],
    ? <: EdgeAnnotatedNFA[String,Char,Int,Set[Int]],
    Elements.AnnotatedNFAelement[String,Char,Int]
  ] = {
    import org.maraist.fa.annotated.setCombiner
    import org.maraist.fa.elements.*
    import org.maraist.fa.annotated.Elements.*

    val builder = EdgeAnnotatedNFA.newBuilder[String, Char, Int, Set[Int]]
    builder += AddInitialState("S")
    builder += AddState("S1")
    builder += AddState("S2a")
    builder += AddState("S2b")
    builder += AddState("S2c")
    builder += AddFinalState("SF")
    builder += AddTransition("S", 'a', "S1")
    builder += SetAnnotation("S", 'a', "S1", 4)
    builder += AddTransition("S1", 'b', "S2a")
    builder += SetAnnotation("S1", 'b', "S2a", 10)
    builder += AddTransition("S1", 'b', "S2b")
    builder += SetAnnotation("S1", 'b', "S2b", 20)
    builder += AddTransition("S2a", 'c', "SF")
    builder += SetAnnotation("S2a", 'c', "SF", 5)
    builder += AddTransition("S2b", 'd', "SF")
    builder += SetAnnotation("S2b", 'd', "SF", 5)
    builder += AddETransition("S2b", "S2c")
    builder += SetEAnnotation("S2b", "S2c", 15)
    builder += AddTransition("S2c", 'd', "S2b")
    builder += SetAnnotation("S2c", 'd', "S2b", 25)
    // builder.dump()
    builder
  }
   */

  /**
  def ann01_nfa: EdgeAnnotatedNFA[String, Char, Int, Set[Int], ? <: EdgeAnnotatedDFA[Set[String], Char, Set[Int]]] = {
    val builder = ann01_builder
    val res = builder.result()
    // res.dump()
    res
  }
   */

  /**
  def ann01_dfa: EdgeAnnotatedDFA[Set[String], Char, Set[Int]] = {
    val res = ann01_nfa.toDFA
    // res.dump()
    res
  }
   */

  /**
  def ann02_nfa: EdgeAnnotatedNFA[String, Char, Int, Set[Int], ?] = {
    import org.maraist.fa.annotated.setCombiner
    import org.maraist.fa.elements.*
    import org.maraist.fa.annotated.Elements.*

    val builder = new EdgeAnnotatedNFABuilder[String, Char, Set[Int], Int]
    builder += AddInitialState("S")
    builder += AddState("S1")
    builder += AddState("S2")
    builder.result()
  }
    */

  def samplesFromNfaBuilder[
    S, T, ThisDFA <: DFA[Set[S],T],
    ThisNFA <: NFA[S,T],
    K >: NFAelements[S,T] <: Matchable
  ](
    doc: LaTeXdoc,
    cleaner: FilesCleaner,
    builder: NFABuilder[S, T],
    tag: String,
    width: String
  ): Unit = {
    doc ++= "\\clearpage\n"
    graphable(
      doc, cleaner, builder, tag + "Builder", tag + " NFA builder", width)
    val nfa = builder.result()
    graphable(doc, cleaner, nfa, tag + "NFA", tag + " NFA",  width)
    val dfa = nfa.toDFA
    graphable(doc, cleaner, dfa, tag + "DFA", tag + " DFA",  width)
  }

  override def addSamples(guide: LaTeXdoc): FilesCleaner = {
    val cleaner = newCleaner()
    section(guide,"Package FA")

    graphable(guide,cleaner, dfa1B,    "dfa1B",    "dfa1B",     "1.75in")

    println("dfa1")
    dfa1.dump
    graphable(guide,cleaner,dfa1,     "dfa1",     "dfa1",      "4in")
    // println(" - back")
    samplesFromNfaBuilder(guide,cleaner,ndfa2B, "ndfa2B", "4in")

//    guide ++= "\\clearpage\n"
//    graphable(guide, cleaner, hdfa3B, "hdfa3B", "hdfa3B", "5in")
//    graphable(guide, cleaner, hdfa3,  "hdfa3",  "hdfa3",  "5in")

//    samplesFromNfaBuilder(guide,cleaner,hndfa4B, "hndfa4", "5in")

//    guide ++= "\\clearpage\n"
//    graphable(guide,cleaner,dlhPfa57, "dlhPfa57", "dlhPfa57",  "3in")
//    graphable(guide,cleaner,dlhPfa57_erem, "dlhPfa57er", "dlhPfa57er",  "3in")

    // samplesFromNfaBuilder(guide, cleaner, ann01_builder, "ann01", "4in")
//    guide ++= "\\clearpage\n"
//    graphable(guide,cleaner,ann01_nfa, "ann01NFA", "ann01NFA",  "6in")
//    graphable(guide,cleaner,ann01_dfa, "ann01.toDFA", "ann01.toDFA",  "8in")

//    guide ++= "\\clearpage\n"
//    graphable(guide,cleaner,ann02_nfa, "ann02NFA", "ann02NFA",  "2in")

    cleaner
  }

  @main def printSamples = {
    val guide = new LaTeXdoc("samples")
    guide.addPackage("geometry", "margin=1in")
    guide.addPackage("times")
    guide.addPackage("graphicx")
    guide.addPackage("multicol")
    guide.open()
    // guide ++= "\\begin{multicols}{2}"
    println("Calling addSamples")
    val cleanup = addSamples(guide)
    println("Back from addSamples")
    // guide ++= "\\end{multicols}"
    guide.close()
    cleanup.clean
  }
}
