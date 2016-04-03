# README #

* Skript R pro archivaci zvoleného účtu na Twitteru
* Jednorázově načte posledních 3200 záznamů (maximum dané knihovnou/API)
* Dalo by se jít ještě víc do minulosti, ale bylo by pak nutné stránkovat - možná TODO
* Při dalších spuštěních skript načítá jen nové tweety a přidává je k původní stažené databázi
* Výsledek uloží do souboru CSV pro ruční načtení do sdílené tabulky na Google Disku
* Tabulka by se dala aktualizovat přímo z R (knihovna googlesheets), ale je to pomalé a jsou problémy s kódováním
