// Copyright (C) 2017, 2021 John Maraist
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied, for NON-COMMERCIAL use.  See the License for the specific
// language governing permissions and limitations under the License.

package org.maraist.fa
import scala.language.adhocExtensions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import org.maraist.fa.impl.{HashDFABuilder,HashNDFABuilder}
import org.maraist.fa.DFA
import org.maraist.fa.pfa.PFA
import org.maraist.fa.pfa.impl.HashPFABuilder
import org.maraist.fa.samples.*

class TestDFA extends AnyFlatSpec with Matchers {

  def assurePFAnormalized[S,T](pfa:PFA[S,T]) = {
    val totals = pfa.getEdgeProbTotals

    for ((s,tmap) <- totals) {
      for ((t,prob) <- tmap) {
        if (prob>0.0)
          assert(prob === 1.0 +- 0.000001)
      }
    }
  }

  def pdaBuilder1:HashPFABuilder[String,Int] = {
    val builder = new HashPFABuilder[String,Int]
    builder.addInitialState("A", 1.0)
    builder.addState("B")
    builder.addState("C")
    builder.addFinalState("D", 0.5)
    builder.addTransition("A", 1, "B", 0.5)
    builder.addTransition("A", 2, "C", 0.5)
    builder.addTransition("B", 3, "D", 0.6)
    builder
  }

  def pdaBuilder2:HashPFABuilder[String,Int] = {
    val builder = new HashPFABuilder[String,Int]
    builder.addInitialState("A", 1.0)
    builder.addState("B")
    builder.addState("C")
    builder.addFinalState("D", 0.5)
    builder.addTransition("A", 1, "B", 0.5)
    builder.addTransition("A", 1, "C", 0.25)

    builder.addTransition("A", 2, "C", 0.5)

    builder.addTransition("B", 3, "A", 0.6)
    builder.addTransition("B", 3, "D", 0.6)
    builder
  }

  "Converting from PDABuilder to PDA" `should` "normalize weights" in {
    val builder = pdaBuilder2
    val pfa = builder.toPFA

    assert(pfa.transition("A",1,"A") === 0.0)
    assert(pfa.transition("A",1,"B") === 0.4 +- 0.000000001)
    assert(pfa.transition("A",1,"C") === 0.2 +- 0.000000001)
    assert(pfa.transition("A",1,"D") === 0.0)

    assert(pfa.transition("A",2,"A") === 0.0)
    assert(pfa.transition("A",2,"B") === 0.0)
    assert(pfa.transition("A",2,"C") === 0.4 +- 0.000000001)
    assert(pfa.transition("A",2,"D") === 0.0)

    assert(pfa.transition("A",3,"A") === 0.0)
    assert(pfa.transition("A",3,"B") === 0.0)
    assert(pfa.transition("A",3,"C") === 0.0)
    assert(pfa.transition("A",3,"D") === 0.0)

    assert(pfa.transition("B",1,"A") === 0.0)
    assert(pfa.transition("B",1,"B") === 0.0)
    assert(pfa.transition("B",1,"C") === 0.0)
    assert(pfa.transition("B",1,"D") === 0.0)

    assert(pfa.transition("B",2,"A") === 0.0)
    assert(pfa.transition("B",2,"B") === 0.0)
    assert(pfa.transition("B",2,"C") === 0.0)
    assert(pfa.transition("B",2,"D") === 0.0)

    assert(pfa.transition("B",3,"A") === 0.5 +- 0.000000001)
    assert(pfa.transition("B",3,"B") === 0.0)
    assert(pfa.transition("B",3,"C") === 0.0)
    assert(pfa.transition("B",3,"D") === 0.5 +- 0.000000001)
  }

  "A PFABuilder" `should` "have the states and transitions put into it" in {
    val builder = pdaBuilder1

    builder.isState("A") `should` be (true)
    builder.isState("B") `should` be (true)
    builder.isState("C") `should` be (true)
    builder.isState("D") `should` be (true)
    builder.isState("E") `should` be (false)

    val dfa = builder.toPFA
    dfa.isState("A") `should` be (true)
    dfa.isState("B") `should` be (true)
    dfa.isState("C") `should` be (true)
    dfa.isState("D") `should` be (true)
    dfa.isState("E") `should` be (false)
  }

  "A DFABuilder" `should` "have the states and transitions put into it" in {
    val builder = new HashDFABuilder[String,Int]("A")
    builder.addState("B")
    builder.addState("C")
    builder.addFinalState("D")
    builder.addTransition("A", 1, "B")
    builder.addTransition("A", 2, "C")
    builder.addTransition("B", 3, "D")

    builder.isState("A") `should` be (true)
    builder.isState("B") `should` be (true)
    builder.isState("C") `should` be (true)
    builder.isState("D") `should` be (true)
    builder.isState("E") `should` be (false)

    val dfa = builder.toDFA
    dfa.isState("A") `should` be (true)
    dfa.isState("B") `should` be (true)
    dfa.isState("C") `should` be (true)
    dfa.isState("D") `should` be (true)
    dfa.isState("E") `should` be (false)

    dfa.accepts(List(1,3)) `should` be (true)
    dfa.accepts(List(1)) `should` be (false)
    dfa.accepts(List(1,2)) `should` be (false)
  }

  "An NDFABuilder" `should` "work" in {
    val builder = new HashNDFABuilder[String,Int]()
    builder.addInitialState("A")
    builder.addState("B")
    builder.addState("C")
    builder.addFinalState("D")
    builder.addTransition("A", 1, "B")
    builder.addTransition("A", 1, "C")
    builder.addTransition("A", 2, "C")
    builder.addTransition("B", 3, "D")
    builder.addETransition("B", "D")

    builder.isState("A") `should` be (true)
    builder.isState("B") `should` be (true)
    builder.isState("C") `should` be (true)
    builder.isState("D") `should` be (true)
    builder.isState("E") `should` be (false)

    val dfa = builder.toDFA
  }

  "An annotated-NDFABuilder" `should` "work" in {
    val nfa = Samples.ann02_nfa
  }
}

class IntSetsTrackerTest extends AnyFlatSpec with Matchers
