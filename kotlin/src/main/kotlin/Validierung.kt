package de.activegroup

/*
 * Regel hat Eigenschaften:
 * - Nummer
 * - Status ("Fehler", "Warnung", "Information")
 * - Beschreibung
 * - Kriterium <-
 */

/*
interface Kriterium {
    fun schlaegtAn(): Boolean
}
 */

enum class Geschlecht {
    MAENNLICH, WEIBLICH // ???
}

enum class BeziehungsStatus {
    LEDIG, GESCHIEDEN, VERHEIRATET, EINGETRAGEN, VERWITWET
}

/*
sealed interface BeziehungsStatus
data object LEDIG: BeziehungsStatus
data class VERHEIRATET(val partnerName: String): BeziehungsStatus
...

"Make illegal states unrepresentable."
Yaron Minsky
 */

data class Person(val name: String,
                  val geschlecht: Geschlecht,
                  val beziehungsStatus: BeziehungsStatus,
                  val partnerName: String?)

sealed interface Kriterium

interface GemeindeKontext {
    fun findePerson(name: String): Person
    // ...
}

sealed interface ImGemeindeKontext<A> {
    fun apply(gemeindeKontext: GemeindeKontext): A
    // "ein anderes Ding"
    // Funktor
//    fun <B> map(f: (A) -> B): ImGemeindeKontext<B> {
//        val zis = this
//        return object: ImGemeindeKontext<B> {
//            override fun apply(gemeindeKontext: GemeindeKontext): B =
//                f(zis.apply(gemeindeKontext))
//        }
//    }
    // Monade
//    fun <B> flatMap(f: (A) -> ImGemeindeKontext<B>): ImGemeindeKontext<B> {
//        val zis = this
//        return object: ImGemeindeKontext<B> {
//            override fun apply(gemeindeKontext: GemeindeKontext): B =
//                f(zis.apply(gemeindeKontext)).apply(gemeindeKontext)
//        }
//    }
    companion object {
        fun <A>pure(wert: A): ImGemeindeKontext<A> = IGKPure(wert)
    }
}

data class IGKPure<A>(val wert: A): ImGemeindeKontext<A> {
    override fun apply(gemeindeKontext: GemeindeKontext): A = wert
}

fun <A, B> ImGemeindeKontext<A>.map(f: (A) -> B): ImGemeindeKontext<B> =
    IGKMap(this, f)

data class IGKMap<A, B>(val igk: ImGemeindeKontext<A>, val f: (A) -> B): ImGemeindeKontext<B> {
    override fun apply(gemeindeKontext: GemeindeKontext): B =
        f(igk.apply(gemeindeKontext))
}

fun <A, B> ImGemeindeKontext<A>.flatMap(f: (A) -> ImGemeindeKontext<B>) =
    IGKFlatMap(this, f)

data class IGKFlatMap<A, B>(val igk: ImGemeindeKontext<A>, val f: (A) -> ImGemeindeKontext<B>): ImGemeindeKontext<B> {
    override fun apply(gemeindeKontext: GemeindeKontext): B =
        f(igk.apply(gemeindeKontext)).apply(gemeindeKontext)
}

class IGKFindePerson(val name: String): ImGemeindeKontext<Person> {
    override fun apply(gemeindeKontext: GemeindeKontext): Person =
        gemeindeKontext.findePerson(name)
}

fun findePerson(name: String): ImGemeindeKontext<Person> = IGKFindePerson(name)

data class IGKNicht(val igk: ImGemeindeKontext<Boolean>): ImGemeindeKontext<Boolean> {
    override fun apply(gemeindeKontext: GemeindeKontext): Boolean =
        !igk.apply(gemeindeKontext)
}

fun ImGemeindeKontext<Boolean>.nicht() = IGKNicht(this)

data class IGKUnd(val igk1: ImGemeindeKontext<Boolean>, val igk2: ImGemeindeKontext<Boolean>):
        ImGemeindeKontext<Boolean> {
    override fun apply(gemeindeKontext: GemeindeKontext): Boolean =
        igk1.apply(gemeindeKontext) && igk2.apply(gemeindeKontext)
}

fun ImGemeindeKontext<Boolean>.und(andereBerechnung: ImGemeindeKontext<Boolean>):
        ImGemeindeKontext<Boolean> =
    IGKUnd(this, andereBerechnung)

data class IGKOder(val igk1: ImGemeindeKontext<Boolean>, val igk2: ImGemeindeKontext<Boolean>):
    ImGemeindeKontext<Boolean> {
    override fun apply(gemeindeKontext: GemeindeKontext): Boolean =
        igk1.apply(gemeindeKontext) && igk2.apply(gemeindeKontext)
}

fun ImGemeindeKontext<Boolean>.oder(andereBerechnung: ImGemeindeKontext<Boolean>):
        ImGemeindeKontext<Boolean> =
    IGKOder(this, andereBerechnung)

interface ImGemeindeKontextOperations {
    fun <A> pure(wert: A): ImGemeindeKontext<A> =
        IGKPure(wert)
}

data class PersonKriterium(val p: suspend ImGemeindeKontextOperations.(Person) -> ImGemeindeKontext<Boolean>): Kriterium

// p: Foo(...) -> ... : Die Funktion muß Foo implementieren

/*
data object Partnerbeziehung: Kriterium
data object ReferenzierterPartner: Kriterium

val h401 = Und(Partnerbeziehung, Nicht(ReferenzierterPartner))
*/

data class Nicht(val kriterium: Kriterium): Kriterium
data class Und(val kriterium1: Kriterium, val kriterium2: Kriterium): Kriterium

val k401 = PersonKriterium { person ->
    pure(person.beziehungsStatus == BeziehungsStatus.EINGETRAGEN ||
        person.beziehungsStatus == BeziehungsStatus.VERHEIRATET).und(
        ImGemeindeKontext.pure(person.partnerName != null).nicht())
}

val h402 = PersonKriterium { person ->
    ImGemeindeKontext.pure(person.beziehungsStatus == BeziehungsStatus.VERHEIRATET)
        .und(findePerson(person.partnerName!!).map { it.geschlecht != person.geschlecht }.oder(
    findePerson(person.partnerName!!).map { it.geschlecht == person.geschlecht }))
}

val h404 = PersonKriterium { person ->
    //val foo: ImGemeindeKontext<Person> =
        // brauchen was wie map, aber statt (A) -> B brauchen wir
        // (A) -> ImGemeindeKontext<B>
        // findePerson(person.partnerName!!).map { partner -> findePerson(partner.partnerName!!) }
    findePerson(person.partnerName!!).flatMap { partner ->
    findePerson(partner.partnerName!!) }.map { partnerPartner ->
            partnerPartner != person
    }
}
