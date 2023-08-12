## SIconfronta

<!-- badges: start -->

[![R-CMD-check](https://github.com/andreabz/SIconfronta/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/andreabz/SIconfronta/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/andreabz/SIconfronta/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/andreabz/SIconfronta/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/andreabz/SIconfronta/branch/master/graph/badge.svg?token=XLFI6Z4IBG)](https://codecov.io/gh/andreabz/SIconfronta)
<!-- badges: end -->

Il software SIconfronta è stato sviluppato per ARPAL allo scopo di
effettuare confronti di medie e varianze tra:

- due serie di misure;
- una serie di misure e un valore noto esattamente;
- due valori dotati di incertezze estese.

Il software permette di sottoporre a test sequenziali più di un analita,
riassumendo i risultati in un unico report in formato pdf.

#### Cosa può fare SIconfronta?

- ottenere statistiche di base sulle serie di valori;
- effettuare un test di normalità (Shapiro-Wilk) sulle serie di valori;
- identificare mediante il test *generalized extreme studentized
  deviate* (GESD) la presenza di potenziali valori anomali;
- escludere manualmente i valori potenzialmente anomali;
- effettuare un test ( $t$-test, Welch test o calcolo dell’ $E_n$) per
  il confronto delle medie;
- effettuare un test ( $F$-test o $\chi^2$-test) per il confronto delle
  varianze;
- esportare i risultati in un report .pdf.

#### Di cosa ha bisogno SIconfronta?

Un file .csv con il punto (.) come separatore decimale e la virgola (,)
come separatore di campo. Ogni colonna presente nel file deve avere
un’intestazione testuale ma il numero e la tipologia di colonne dipende
dallo scopo:

<details>
<summary>
se vuoi confrontare due serie di misure di cui possiedi i singoli valori
per entrambe le serie, clicca qui
</summary>

Ti servirà organizzare il file con:

- una colonna testuale con i nomi degli analiti di interesse;
- una colonna testuale con due soli valori, di modo da permettere
  l’identificazione del gruppo di appartenenza dei singoli valori;
- una colonna numerica con i valori delle misure.

Ognuna delle due serie potrà avere da un minimo di 5 a un massimo di 30
valori per ogni coppia formata dall’identificativo dell’analita e del
gruppo

</details>
<details>
<summary>
se vuoi confrontare due serie di misure di cui possiedi i singoli valori
per una sola delle serie, clicca qui
</summary>

Ti servirà organizzare il file con: \* una colonna testuale con i nomi
degli analiti di interesse; \* una colonna testuale con il nome del
gruppo di appartenenza dei valori; \* una colonna numerica con i valori
delle misure.

Inoltre, nel corso dell’esecuzione dei test, per la serie di misure per
cui non si dispongono i singoli valori, si dovranno inserire:

- il nome del gruppo;
- il valore della media;
- il valore di deviazione standard;
- la numerosità.

La serie per cui sono disponibili le singole misure, potrà avere da un
minimo di 5 a un massimo di 30 valori per ogni coppia formata
dall’identificativo dell’analita e del gruppo.

</details>
<details>
<summary>
se vuoi confrontare una serie di misure con un valore noto, clicca qui
</summary>

Ti servirà organizzare il file con:

- una colonna testuale con i nomi degli analiti di interesse;
- una colonna testuale con il nome del gruppo di appartenenza dei valori
  disponibili;
- una colonna numerica con i valori delle misure.

Inoltre, nel corso dell’esecuzione dei test si dovranno inserire:

- il nome da assegnare al valore noto;
- il valore noto con cui operare il confronto.

La serie di misure potrà avere da un minimo di 5 a un massimo di 30
valori per ogni coppia formata dall’identificativo dell’analita e del
gruppo.

</details>
<details>
<summary>
se vuoi confrontare due misure dotate di incertezza estesa, clicca qui
</summary>

Ti servirà organizzare il file con:

- una colonna testuale con i nomi degli analiti di interesse;
- una colonna testuale con il nome del gruppo di appartenenza dei valori
  disponibili;
- una colonna numerica con i valori delle misure;
- una colonna numerica con i valori delle incertezze estese delle
  misure.

Può essere presente solo un valore per ogni coppia formata
dall’identificativo dell’analita e del gruppo

</details>

<br>

#### Come sono stati scelti i test di Calibrat?

Il test per verificare la normalità di una serie di dati è stato
implementato utilizzando la funzione `shapiro.test` della libreria
`stats` del software R. L’implementazione risulta coerente con quanto
riportato nella norma ISO 5479:1997 e in grado di replicare i risultati
contenuti nell’esempio 1 della sezione 4 (pag. 606) dell’articolo
Shapiro, S. S. e Wilk, M. B. (1965). *An analysis of variance test for
normality (complete samples)*, Biometrika, *52*, 3 and 2. doi:
[10.2307/2333709](https://doi.org/10.2307/2333709)

Il test *generalized extreme studentized deviate* (GESD) per
l’identificazione dei valori anomali è stato implementato sulla base del
paragrafo 4.3 e dell’Annex A della norma UNI ISO 16269-4:2019. Il test è
stato in grado di replicare i risultati riportati come esempio nella
medesima norma (pag. 15).

I test per il confronto tra due medie e tra una media e un valore noto
sono stati implementati utilizzando la funzione `t.test` della libreria
`stats` del software R. L’implementazione risulta coerente e in grado di
replicare i risultati riportati nella norma UNI ISO 2854:1988 (prospetti
B’, C’ e D’). Tuttavia, rispetto a quanto riportato nella norma, le
varianze dei due set di dati vengono supposte tra loro diverse, portando
all’applicazione del *t*-test nella sua versione detta [Welch
test](https://en.wikipedia.org/wiki/Welch%27s_t-test). Tale test si è
rivelato maggiormente efficace rispetto al *t*-test classico nel
contenere gli errori di tipo I nel caso di varianze tra loro non uguali.
Tale capacità si è evidenziata anche in situazioni in cui la differenza
tra le varianze non sia sufficiente ad essere rilevata da un test sulla
varianza di Fisher (tipicamente poco potente per numerosità campionarie
ridotte). Ulteriori informazioni in merito sono disponibili nei seguenti
articoli:

- Welch, B. L. (1951). *On the Comparison of Several Mean Values: An
  Alternative Approach*. Biometrika. *38* (3/4): 330–336. doi:
  [10.2307/2332579](https://doi.org/10.2307%2F2332579)
- Zimmerman, D. W. (2004). *A note on preliminary tests of equality of
  variances*. British Journal of Mathematical and Statistical
  Psychology. *57* (Pt 1): 173–181. doi:
  [10.1348/000711004849222](https://doi.org/10.1348%2F000711004849222)

Il test per il confronto tra una varianza e un valore assegnato è stato
implementato sulla base del test del $\chi^2$ riportato nel prospetto E
della norma UNI ISO 2854:1988. Il test implementato è stato in grado di
replicare i risultati riportati nella medesima norma.

Il test per il confronto tra due varianze è stato implementato
utilizzando la funzione `var.test` della libreria `stats` del software
R. L’implementazione risulta coerente e in grado di fornire risultati in
linea con quanto riportato nella norma UNI ISO 2854:1988 (prospetti G e
H).

Il test per il confronto di due valori dotati di incertezza estesa, il
calcolo dell’ $E_n$ è stato implementato sulla base di quanto riportato
nella norma ISO 13528:2022, al paragrafo 9.7. Il calcolo implementato è
stato in grado di replicare i risultati riportati nella sezione E.4.
della medesima norma.

#### Come funziona Calibrat?

L’applicazione è suddivisa in quattro schede con cui l’utente deve
interagire in sequenza. Ogni scheda possiede un pulsante *Avanti*
posizionato in basso a sinistra, una volta cliccato su di esso e
confermata la propria scelta, l’utente non potrà tornare indietro.

<details>
<summary>
Dalla scheda <b> Scopo </b>, clicca qui
</summary>

1.  selezionare una delle opzioni disponibili;
2.  leggere le istruzioni nella parte a destra dello schermo;
3.  cliccare su *Avanti* e confermare la propria scelta.

</details>
<details>
<summary>
Dalla scheda <b> Dati </b>, clicca qui
</summary>

1.  leggere le istruzioni nella parte destra dello schermo;
2.  caricare il file .csv;
3.  controllare e selezionare le variabili di interesse nei menù a
    tendina;
4.  cliccare su *Avanti* e confermare la propria scelta.

</details>
<details>
<summary>
Dalla scheda <b> Confronti </b>, clicca qui
</summary>

1.  leggere le istruzioni nella parte a destra dello schermo;
2.  selezionare il parametro di interesse;
3.  digitare le unità di misura;
4.  digitare le eventuali altre informazioni richieste e, se presente,
    cliccare *Calcola*;
5.  specificare l’ipotesi alternativa per i test e il loro livello di
    confidenza;
6.  visualizzare il grafici e le statistiche di base;
7.  eventualmente rimuovere dei punti cliccando su di essi;
8.  visualizzare gli esiti dei test spostandosi tra le schede nella
    parte destra dello schermo;
9.  cliccare su *Salva* per salvare il risultato;
10. ripetere i punti dal 2. al 9. per tutti gli analiti di interesse;
11. cliccare su *Avanti* e confermare la propria scelta.

Nel caso si voglia modificare un risultato già salvato:

1.  accedere al menù a tendina in alto;
2.  selezionare il parametro di interesse;
3.  cliccare su *Cancella*;
4.  fare le modifiche volute;
5.  cliccare su *Salva*;
6.  seguire i punti 10. e 11. dell’elenco puntato precedente.

</details>
<details>
<summary>
Dalla scheda <b> Report </b>, clicca qui
</summary>

1.  completare i campi con le informazioni accessorie;
2.  selezionare le sezioni da includere nel report;
3.  cliccare su *Crea il report*;
4.  aspettare che il file .pdf compaia tra i file scaricati.

</details>

<br>

#### SIconfronta è validato?

SIconfronta è basato sull’ambiente software R e molte delle sue funzioni
sono utilizzate in ambito professionale da milioni di persone, da circa
30 anni.

La correttezza dei risultati forniti dalle funzioni impiegate da
SIconfronta, l’interazione tra le funzioni e la stabilità
dell’interfaccia utente, sono oggetto di oltre 650 test. Tali test sono
eseguiti in automatico a ogni nuovo rilascio di versione. La frazione di
codice coperta dai test è circa il 93% del totale. L’esito dei controlli
e la percentuale di codice coperta dai test per l’ultima versione
rilasciata, è riassunto nelle etichette aggiornate automaticamente e
presenti a inizio di questo file.

Si consiglia, inoltre, di rendere disponibile SIconfronta attraverso un
server Ubuntu ad accesso controllato o distribuirlo mediante *docker*.

A ogni modo, è sempre meglio rimanere allerta: [segnala eventuali
bachi](mailto:andrea.bazzano@arpal.liguria.it).

#### Con quale licenza è rilasciato SIconfronta?

Con la Affero GPL versione 3.

[![gplv3](https://www.gnu.org/graphics/agplv3-with-text-100x42.png)](https://www.gnu.org/graphics/agplv3-with-text-100x42.png)

Il testo integrale della licenza è disponibile a questo indirizzo:
<https://www.gnu.org/licenses/agpl-3.0.en.html#license-text>.

Copyright (C) 2023, Andrea Bazzano, <andrea.bazzano@arpal.liguria.it>.
