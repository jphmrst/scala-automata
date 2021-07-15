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
import scala.collection.mutable.{Builder,HashMap,HashSet}
import org.maraist.util.FilesCleaner
import org.maraist.fa.{DFA, NDFA, NDFABuilder}
import org.maraist.fa.impl.{HashNDFABuilder}
import org.maraist.fa.elements.
  {AddState, RemoveState, RemoveFinalState, AddFinalState,
    RemoveTransition, AddTransition}
import org.maraist.fa.DFA.{DFAelements}
import org.maraist.fa.pfa.PFABuilder
import org.maraist.fa.pfa.impl.HashPFABuilder
import org.maraist.fa.hyperedges.
  {HyperedgeDFA, HyperedgeDFABuilder, HyperedgeNDFA, HyperedgeNDFABuilder,
    IndexedHyperedgeDFA}
import org.maraist.fa.hyperedges.Builders.HyperedgeDFAelements
import org.maraist.fa.hyperedges.impl.
  {ArrayHyperedgeDFA, HashHyperedgeDFABuilder, HashHyperedgeNDFABuilder}
import org.maraist.fa.annotated.EdgeAnnotatedNDFA
import org.maraist.latex.{LaTeXdoc,Sampler}

/**
 * Sample automata, and printing a guide to them.
 */
object Samples extends Sampler {

  /**
   * Return a fresh copy of a sample DFA builder
   */
  def dfa1B: Builder[DFAelements[String,Int], DFA[String,Int]] = {
    val builder = DFA.newBuilder[String,Int]("A")
    builder += AddState[String,Int]("B")
    builder += AddState("C")
    builder += AddFinalState("D")
    builder += AddTransition("A", 1, "B")
    builder += AddTransition("A", 2, "C")
    builder += AddTransition("B", 3, "D")

    // val builder = new HashDFABuilder[String,Int]("A")
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
   * Return a fresh copy of a sample NDFA builder
   */
  def ndfa2B: NDFABuilder[String,Int,?,?,?] = {
    val builder = new HashNDFABuilder[String,Int]()
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
   * Return an NDFA, derived from a fresh copy of the sample builder
   * {@link ndfa2B}
   */
  def ndfa2: NDFA[String,Int,?] = {
    ndfa2B.toNDFA
  }

  /**
   * Return a DFA converted from a fresh copy of the sample NDFA builder
   * {@link ndfa2B}
   */
  def ndfa2dfa: DFA[Set[String],Int] = {
    ndfa2.toDFA
  }

  /**
   * Return a fresh copy of a sample DFA-with-hyperedge builder
   */
  def hdfa3B: HyperedgeDFABuilder[
    String, Int, ArrayHyperedgeDFA[String,Int],
    HyperedgeDFAelements[String,Int]
  ] = {
    val builder = new HashHyperedgeDFABuilder[String,Int]("A")
    builder.addState("B")
    builder.addState("C")
    builder.addFinalState("D")
    builder.addTransition("A", 1, "B")
    builder.addTransition("A", 2, "C")
    builder.addTransition("B", 3, "D")
    builder.addEHyperedge("C", Set[String]("B", "D"))
    builder
  }

  /**
   * Return a DFA converted from a fresh copy of the sample
   * DFA-with-hyperedge builder {@link hdfa3B}
   */
  def hdfa3: HyperedgeDFA[String,Int] = {
    val res: ArrayHyperedgeDFA[String,Int] = hdfa3B.result()
    res
  }

  /**
   * Return a fresh copy of a sample NDFA-with-hyperedge builder
   */
  def hndfa4B: HyperedgeNDFABuilder[String, Int, IndexedHyperedgeDFA[Set[String], Int], HyperedgeNDFA[String,Int, IndexedHyperedgeDFA[Set[String], Int]]] = {
    val builder = new HashHyperedgeNDFABuilder[String,Int]
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

  /**
   * Return a NDFA converted from a fresh copy of the sample
   * NDFA-with-hyperedge builder {@link hndfa4B}
   */
  def hndfa4: HyperedgeNDFA[String, Int, IndexedHyperedgeDFA[Set[String], Int]] = {
    hndfa4B.toNDFA
  }

  /**
   * Return a DFA converted from a fresh copy of the sample
   * NDFA-with-hyperedge builder {@link hndfa4B}
   */
  def hndfa4dfa: HyperedgeDFA[Set[String],Int] = {
    hndfa4.toDFA
  }

  def dlhPfa57:PFABuilder[Int,String] = {
    val res = new HashPFABuilder[Int,String]
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

  def dlhPfa57_erem: PFABuilder[Int,String] = {
    val res = dlhPfa57
    res.removeEpsilonTransitions
    res
  }

  def ann01_nfa: EdgeAnnotatedNDFA[String, Char, Int, Set, ?] = {
    import org.maraist.fa.annotated.setCombiner
    import org.maraist.fa.elements.*
    import org.maraist.fa.annotated.Elements.*

    val builder = EdgeAnnotatedNDFA.newBuilder[String, Char, Int, Set]
    // builder += AddInitialState("S")
    builder += AddState("S1")
    // builder += AddState("S2")
    builder.result()
  }

  def addSamples(guide:LaTeXdoc):FilesCleaner = {
    val cleaner = newCleaner()
    section(guide,"Package FA")

    // This one uses the Builder API, so loses the relationship to Graphable
    // graphable(guide,cleaner,dfa1B,    "dfa1B",    "dfa1B",     "1.75in")

    graphable(guide,cleaner,dfa1,     "dfa1",     "dfa1",      "1.75in")
    graphable(guide,cleaner,ndfa2B,   "ndfa2B",   "ndfa2B",    "1.75in")
    graphable(guide,cleaner,ndfa2,    "ndfa2",    "ndfa2",     "1.75in")
    graphable(guide,cleaner,ndfa2dfa, "ndfa2dfa", "ndfa2dfa",  "3in")
    graphable(guide,cleaner,hdfa3B,   "hdfa3B",   "hdfa3B",    "3in")
    graphable(guide,cleaner,hdfa3,    "hdfa3",    "hdfa3",     "3in")
    graphable(guide,cleaner,hndfa4B,  "hndfa4B",  "hndfa4B",   "3in")
    graphable(guide,cleaner,hndfa4,   "hndfa4",   "hndfa4",    "3in")
    graphable(guide,cleaner,hndfa4dfa,"hndfa4dfa","hndfa4dfa", "3in")
    graphable(guide,cleaner,dlhPfa57, "dlhPfa57", "dlhPfa57",  "3in")
    graphable(guide,cleaner,dlhPfa57_erem, "dlhPfa57er", "dlhPfa57er",  "3in")

    // // Rendering traverser for annotated automata not yet implemented.
    // graphable(guide,cleaner,ann01_nfa, "ann01NFA", "ann01NFA",  "2in")

    cleaner
  }

  @main def printSamples = {
    val guide = new LaTeXdoc("samples")
    guide.addPackage("times")
    guide.addPackage("graphicx")
    guide.addPackage("multicol")
    guide.open()
    guide ++= "\\begin{multicols}{2}"
    val cleanup = addSamples(guide)
    guide ++= "\\end{multicols}"
    guide.close()
    cleanup.clean
  }
}
