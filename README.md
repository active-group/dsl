# Domänenspezifische Sprachen 2025-07-23

Wir werden während der Schulung konkretes DSL-Design anhand von
praxisnahen Beispielen üben.  Ich bitte deshalb darum, einen Laptop
mitzubringen und einige Software zu installieren.  Für den ersten Tag
reicht erstmal Racket.

Racket:
https://download.racket-lang.org/

Gegebenenfalls werden wir brauchen:

JDK:
https://www.oracle.com/java/technologies/downloads/

Maven:
https://maven.apache.org/download.cgi

IntelliJ MPS:
https://www.jetbrains.com/mps/

Xtext/Eclipse:
https://www.eclipse.org/downloads/packages/
Dort: "Eclipse IDE for Java and DSL Developers"

Bei Fragen oder Problemen gern Mike Sperber kontaktieren!

# Shared Notes

- Grammatk / Syntax​
- Parser: Code (Abfolge von Buchstaben) -> AST
- Lisp-artige Mechanismen: Syntax
- Builder-Pattern
- textuelle Ersatzung
- Haskell ...?

## Kontext-Mapping AUTOSAR:
    - jmd #1 überlegt sich Format für Tabellen
    - jmd #2 erzeugt Tabellen mit diesem Format
    - jmd #3 integriert Tabellendaten in konkrete AUTOSAR-Modelle
    
## Tabelle als Kombinatormodell:
    - P: EINE Zelle mit "primitivem" Typ  -ODER-
    - P: ein Header -ODER-
    - C: eine Datensatz aus einer festen Anzahl von Tabellen -> Struct/Record -ODER-
    - C: eine Liste aus einer dynamischen Anzahl von Tabellen -> Liste
    
## Fragen:
    - Was macht Kotlin um DSL zu ermöglichen
    - QDSL vs EDSL
    - Wie geht man die Modellierung von grossen Themen an?
    - Scala VS Kotlin
    - Sprachen ohne Klammerprobleme ;-) => MPS?
    - Kosten / Nutzen von Parsern...
    
## Tag #2:
    1. Modell fertigmachen + aufräumen + Tests + Bug fixen
    2. Abstraktion
    3. Tabellen in Kotlin
    4. Eure Beispiele

## Tag #3:
- Option
- Unterbau Validierungsbeispiel
- Scala (Monaden)
- Kotlin-Monade
- Kotlin-DSL
- Macros ("QDSL")
- Syntax
- projektive Syntax

# Material

- [The Next 700 Programming Languages](https://dl.acm.org/doi/pdf/10.1145/363744.363749)
- [Selective Applicative Functors](https://dl.acm.org/doi/10.1145/3341694)
- [Vavr](https://vavr.io/), [Vavr Kotlin](https://github.com/vavr-io/vavr-kotlin)
- [Monaden in Kotlin](https://github.com/active-group/kotlin-free-monad), [Artikel dazu](https://www.sigs.de/artikel/funktionale-programmierung-fuer-bessere-architektur/)
- [Phil Wadler: QDSL](https://homepages.inf.ed.ac.uk/wadler/topics/qdsl.html)
- [Schreibe Dein Programm!](https://www.deinprogramm.de/sdp/) (Kapitel
  zum Lambda-Kalkül)
