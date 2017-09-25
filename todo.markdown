TODO-Liste
==========

  * Behandlung von (externen) Funktionen auf Literalen

    * Annotiere alle (externen) Funktionsaufrufe mit einer expression id
      (theoretisch sollte es ausreichen alle Funktionsaufrufe zu annotieren,
       die verzweigen können)
    * Verwalte während der Auswertung eine Map, die expression ids auf constraints mapt
    * Verwende zusätzlich symbolischen Heap ?

  * Behandlung von Literal-Pattern in case-Ausdrücken (bei partieller Applikation)
     eventuell einfacher lösbar durch FlatCurry-Transformation
  * Müssen case decisions mit nur einem branch getraced werden?
  * Refactoring der Suche, zusätzliche Abstraktion
  * Berücksichtigung von externen Funktionen wie +, -, == etc.
  * verschiedene Strategien für konkolische Suche (naive Tiefensuche, Berücksichtigung
    des bisherigen Funktionsaufrufgraphen vgl. @sebf)
  * Umstellung des Pretty Printers / der SMT-LIB-Repräsentation auf SMT-LIB 2.6
  * Implementierung alternativer coverage Kriterien (call coverage)
