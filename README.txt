Es wurde mit R version 4.05 gearbeitet. 
Folgende Pakete werden benutzt:
-collapsibleTree 0.1.7
-countreg	 0.2-1
-dagitty	 0.3-1
-data.table	 0.14.0
-foreach	 1.5.1
-gender		 0.5.4
-ggdag		 0.2.3
-ggstatsplot	 0.8.0
-gridExtra	 2.3
-httr		 1.4.2
-jsonlite	 1.7.2
-lavaan		 0.6-8
-mice		 3.13.0
-plyr		 1.8.6
-pscl		 1.5.5
-rcrossref	 1.1.0
-readr		 1.4.0
-roadoi		 0.7
-rvest		 1.0.0
-stringr	 1.4.0
-tidyverse	 1.3.1

Alle Datensätze sind im Ordner "Data" zu finden. Der Ordner "Plots" enthält alle 
Plots und Tabellen.

"get_data.R" erzeugt den Datensatz "complete_data" welcher alle Variablen enthält.
"clean_data.R" räumt diesen Datensatz auf (nur nötige Variablen, richtiger type,
nur relevante Publikationen, usw.) und erzeugt den Datensatz "clean_data" welcher
die Grundlage aller Analysen bildet. Diese beiden Dateien müssen nicht ausgeführt
werden, alle weiteren Skripte greifen direkt auf "clean_data" zu.

"scraping_semantic.R" und "get_journal_subject.R" sollten ebenfalls nicht ausgeführt
werden. Sie dienen dazu, dass Webscraping durchzuführen bzw. die Journal 
Unterkategorien den Oberkategorien zuzuordnen. Sie erzeugen eigene Datensätze welche
in "get_data.R" benötigt werden.

"dag.R" erzeugt das DAG und führt die Tests dazu durch.

"imputation.R" führt die Imputation durch, prüft ob diese korrekt durchgeführt wurde
und vergleicht die verschiedenen Methoden miteinander. Dazu werden Funktionen in
"imputation_functions.R" definiert.

"modelling.R" stellt die verschiedenen Modelle auf. Die Koeffizientenschätzer der
Imputationsmodelle werden gepoolt. Die Modelle werden mit Rootogramen verglichen.

"results.R" erzeugt alle Plots.

Datensätze:
-"hmgu_publications": Datensatz aller Publikationen des Helmholtz-Zentrums
-"crossref_df": Datensatz von Crossref
-"unpaywall_df": Datensatz von Unpaywall
-"Scopus_data": ASJC Codes der Journal
-"journals_na_filled": manuelle Zuordnung der Journal Themen
-"complete_journals": Liste aller verwendeten Journals und ihr Thema
-"dois": alle dois (verwendet zum Scraping)
-"h_index": gescrapte h-indizes
-"gender": Geschlecht der Erstautoren
-"data_complete": kompletter Datensatz vor cleaning
-"data_clean": Datensatz nach cleaning, Grundlage aller Analysen
-"imp": 5 Imputationsdatensätze


