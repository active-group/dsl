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

interface ImGemeindeKontext<A> {
    fun apply(gemeindeKontext: GemeindeKontext): A
    // "ein anderes Ding"
    // Funktor
    fun <B> map(f: (A) -> B): ImGemeindeKontext<B> {
        val zis = this
        return object: ImGemeindeKontext<B> {
            override fun apply(gemeindeKontext: GemeindeKontext): B =
                f(zis.apply(gemeindeKontext))
        }
    }
    companion object {
        fun <A>heben(wert: A): ImGemeindeKontext<A> = object: ImGemeindeKontext<A> {
            override fun apply(gemeindeKontext: GemeindeKontext): A = wert
        }
    }
}

fun findePerson(name: String): ImGemeindeKontext<Person> =
    object: ImGemeindeKontext<Person> {
        override fun apply(gemeindeKontext: GemeindeKontext): Person =
            gemeindeKontext.findePerson(name)

    }


fun ImGemeindeKontext<Boolean>.nicht() = object: ImGemeindeKontext<Boolean> {
    override fun apply(gemeindeKontext: GemeindeKontext): Boolean = !this.apply(gemeindeKontext)
}

fun ImGemeindeKontext<Boolean>.und(andereBerechnung: ImGemeindeKontext<Boolean>) =
    object:ImGemeindeKontext<Boolean> {
        override fun apply(gemeindeKontext: GemeindeKontext): Boolean =
            this.apply(gemeindeKontext) && andereBerechnung.apply(gemeindeKontext)
}
fun ImGemeindeKontext<Boolean>.oder(andereBerechnung: ImGemeindeKontext<Boolean>) =
    object:ImGemeindeKontext<Boolean> {
        override fun apply(gemeindeKontext: GemeindeKontext): Boolean =
            this.apply(gemeindeKontext) || andereBerechnung.apply(gemeindeKontext)
    }

data class PersonKriterium(val p: (Person) -> ImGemeindeKontext<Boolean>): Kriterium

/*
data object Partnerbeziehung: Kriterium
data object ReferenzierterPartner: Kriterium

val h401 = Und(Partnerbeziehung, Nicht(ReferenzierterPartner))
*/

data class Nicht(val kriterium: Kriterium): Kriterium
data class Und(val kriterium1: Kriterium, val kriterium2: Kriterium): Kriterium

val k401 = PersonKriterium { person ->
    ImGemeindeKontext.heben(person.beziehungsStatus == BeziehungsStatus.EINGETRAGEN ||
        person.beziehungsStatus == BeziehungsStatus.VERHEIRATET).und(
        ImGemeindeKontext.heben(person.partnerName != null).nicht())
}

val h402 = PersonKriterium { person ->
    ImGemeindeKontext.heben(person.beziehungsStatus == BeziehungsStatus.VERHEIRATET)
        .und(findePerson(person.partnerName!!).map { it.geschlecht != person.geschlecht }.oder(
    findePerson(person.partnerName!!).map { it.geschlecht == person.geschlecht }))
}
