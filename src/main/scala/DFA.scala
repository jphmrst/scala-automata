// Copyright (C) 2017, 2021 John Maraist
// See the LICENSE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import scala.collection.mutable.{Builder, HashMap, HashSet}
import org.maraist.fa.styles.AutomatonStyle
import org.maraist.fa.full

/** Implementation of DFAs.
 *  @tparam S The type of all states of the automaton
 *  @tparam T The type of labels on transitions of the automaton
 *
 * @group DFA
 */
class DFA[S,T](
  protected val stateSeq: IndexedSeq[S],
  val initialStateIndex: Int,
  val finalStateIndices: Set[Int],
  protected val transitionsSeq: IndexedSeq[T],
  protected val transitionsMatrix: Array[Array[Int]]
)

extends full.DFA[S, T, AutomatonStyle] {

//  /** Internal routine used by {@link #toDOT}.  Subclesses should
//   *  override, but still call super.internalsToDOT, to extend the
//   *  Graphviz representation of a DFA */
//  protected def internalsToDOT(stateList: IndexedSeq[S], sb: StringBuilder
//  )(using graphvizOptions: GraphStyle[S, T]):
//      Unit = {
//    // println("        In DFA.dotTraverse with " + graphvizOptions)
//
//    // Initial state
//    sb ++= "\tinit [shape=none, margin=0, label=\"\"];\n"
//
//    traverse(dotTraverser(sb, stateList))
//    for(si0 <- 0 until stateList.length) {
//      val s0 = stateList(si0)
//      for(t <- labels) {
//        transition(s0,t) match {
//          case Some(s1) => {
//          }
//          case None => { }
//        }
//      }
//    }
//  }
//
//  /** {@inheritDoc} */
//  override def toDOT(using options: GraphStyle[S, T]):
//      String = {
//    // println("       In DFA.toDOT with " + options)
//    val stateList = IndexedSeq.from(states)
//    val sb = new StringBuilder()
//    internalsToDOT(stateList, sb)
//    sb.toString()
//  }

//  override def toString(): String = {
//    val bld: StringBuilder = new StringBuilder
//    for (st <- states) {
//      if (isInitialState(st)) bld ++= "> " else bld ++= "  "
//      bld ++= st.toString() + "\n"
//      for (tr <- labels)
//        transition(st, tr) match {
//          case Some(x) => bld ++= ("  - " + tr + " --> " + x + "\n")
//          case None =>
//        }
//    }
//    bld.toString()
//  }

  // ---------------------------------------------------------------
  // Dumping out the DFA as text

//  def dump(): Unit = {
//    dumpHeader()
//    dumpStates()
//    dumpTransitions()
//    dumpOther()
//    dumpFooter()
//  }
//
//  protected def dumpHeader(): Unit = println("---------- DFA dump")
//  protected def dumpFooter(): Unit = println("----------")
//
//  protected def dumpStates(): Unit = {
//    println("States:")
//    for(state <- states) {
//      dumpState(state)
//    }
//  }
//
//  protected def dumpState(s: S): Unit = {
//    print("- " + s)
//    if (isInitialState(s) || isFinalState(s)) print(" (")
//    if (isInitialState(s)) print("initial")
//    if (isInitialState(s) && isFinalState(s)) print(", ")
//    if (isFinalState(s)) print("final")
//    if (isInitialState(s) || isFinalState(s)) print(")")
//    println()
//  }
//
//  protected def dumpTransitions(): Unit = {
//    println("Transitions:")
//    for(src <- states) {
//      for(label <- labels) {
//        transition(src, label) match {
//          case None => { }
//          case Some(dest) => dumpTransition(src, label, dest)
//        }
//      }
//    }
//  }
//
//  protected def dumpTransition(src: S, label: T, dest: S): Unit = {
//    println("- " + src + " -[ " + label + " ]-> " + dest)
//  }
//
//  protected def dumpOther(): Unit = { }
}

/**
  * @group DFA
  *
  * @groupname DFA Forms of DFAs
  * @groupdesc DFA
  * @groupprio DFA 100
  *
  * @groupname builderElements Arguments to
  * [[scala.collection.mutable.Builder Builder]]s for [[DFA]]s
  * @groupdesc builderElements
  * @groupprio builderElements 150
  *
  * @groupname builderPattern [[scala.collection.mutable.Builder Builder]]
  * pattern method
  * @groupdesc builderPattern
  * @groupprio builderPattern 160
  */
object DFA {

//  // Fetch a builder for the pattern.
//
//  /** [[scala.collection.mutable.Builder Builder]] pattern method for
//    * [[DFA]]s.
//    *
//    *  @tparam S The type of all states of the automaton
//    *  @tparam T The type of labels on transitions of the automaton
//    *
//    * @group builderPattern
//    */
//  def newBuilder[S, T](initialState: S) =
//    newBuilderFor[S, T, HashDFABuilder, DFA](initialState)
//
//  /** [[scala.collection.mutable.Builder Builder]] pattern method for
//    * [[DFA]]s.
//    *
//    *  @tparam S The type of all states of the automaton
//    *  @tparam T The type of labels on transitions of the automaton
//    *
//    * @group builderPattern
//    */
//  def newBuilderFor[
//    S, T,
//    Bldr[X,Y] <: Builder[DFAelements[X,Y],Impl[X, Y]],
//    Impl[X, Y] <: DFA[X,Y]
//  ](initialState: S)(using impl: HasBuilderWithInit[DFAelements, Bldr, Impl]) =
//    impl.build[S,T](initialState)
//
//  /** Implementation of [[scala.collection.mutable.Builder Builder]]
//    * pattern for [[DFA]]s.
//    *
//    * @group builderPattern
//    */
//  given hashBd: HasBuilderWithInit[DFAelements, HashDFABuilder, DFA] with {
//    override def build[S,T](init: S): HashDFABuilder[S, T] =
//        new HashDFABuilder[S, T](init)
//  }
}
