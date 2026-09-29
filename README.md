# SIconfronta

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/andreabz/SIconfronta/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/andreabz/SIconfronta/actions/workflows/check-standard.yaml) [![test-coverage](https://github.com/andreabz/SIconfronta/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/andreabz/SIconfronta/actions/workflows/test-coverage.yaml)

<!-- badges: end -->

**Shiny application for the statistical comparison of analytical measurement results.**

SIconfronta è un'applicazione sviluppata in R e Shiny per supportare il **confronto statistico tra risultati di misura**, con particolare riferimento al confronto tra medie, varianze e valori accompagnati da incertezza di misura.

L'applicazione permette di analizzare più analiti consecutivamente e di raccogliere i risultati in un report PDF.

Il progetto è sviluppato da **ARPAL – Agenzia Regionale per la Protezione dell'Ambiente Ligure** e utilizza R/Shiny per rendere espliciti e riproducibili i calcoli statistici alla base dei confronti.

## A cosa serve

Il confronto tra risultati analitici può assumere forme diverse a seconda delle informazioni disponibili.

SIconfronta è stato progettato per gestire, tra gli altri, i seguenti casi:

* confronto tra **due serie indipendenti di misure**;
* confronto tra una serie di misure e un **valore di riferimento**;
* confronto tra due risultati accompagnati dalle rispettive **incertezze di misura**;
* confronto della **media** di una serie con un valore assegnato;
* confronto della **variabilità** di due serie di misure.

L'obiettivo non è fornire un unico test statistico per qualsiasi confronto, ma associare il metodo di analisi alla struttura dei dati disponibili e alla domanda specifica.

## Cosa può fare

L'applicazione comprende funzioni per:

* statistiche descrittive di base;
* verifica della normalità mediante **Shapiro-Wilk**;
* individuazione di potenziali valori anomali mediante **GESD**;
* esclusione manuale di osservazioni, quando motivata;
* confronto delle medie mediante **t-test**;
* confronto delle medie mediante **Welch test** quando le varianze possono essere differenti;
* confronto mediante **En** quando sono disponibili valori e relative incertezze;
* confronto delle varianze mediante test **F**;
* confronto di una varianza con un valore assegnato mediante test **chi-quadro**;
* generazione automatica di un report PDF.

L'applicazione è organizzata in una sequenza guidata:

**Scopo → Dati → Confronti → Report**

Una volta confermata una fase, l'interfaccia non consente di tornare alle fasi precedenti. La sequenza rende esplicito il passaggio dalla definizione dello scopo dell'analisi alla selezione dei dati e alla produzione dei risultati.

## Casi di confronto

SIconfronta può essere utilizzato in diversi scenari sperimentali.

### Due serie indipendenti

Quando sono disponibili due serie complete di risultati, l'applicazione permette di confrontare:

* le rispettive medie;
* le rispettive varianze.

La scelta del test dipende dalle caratteristiche delle serie e dall'ipotesi statistica che si vuole verificare.

### Serie e valore noto

Una serie di misure può essere confrontata con un valore assegnato o considerato noto.

In questo caso il confronto riguarda la compatibilità tra il risultato sperimentale e il valore di riferimento secondo il modello statistico previsto.

### Due risultati con incertezza

Quando sono disponibili due risultati di misura corredati dalle rispettive incertezze estese, il confronto può essere effettuato mediante il parametro **En**.

Questo permette di considerare contemporaneamente la differenza tra i valori e le incertezze associate alle due misure.

### Dati riassuntivi

In alcuni casi non è necessario disporre delle singole osservazioni: l'applicazione può utilizzare informazioni riassuntive, come media, deviazione standard e numerosità, quando queste sono sufficienti per il confronto richiesto.

## Dati di ingresso

I dati vengono forniti mediante file CSV.

Il formato prevede:

* separatore dei campi: `,`;
* separatore decimale: `.`;
* uno o più analiti;
* numero di osservazioni dipendente dal tipo di confronto.

A seconda dello scenario, possono essere richiesti:

* serie complete di misure;
* media, deviazione standard e numerosità;
* valori assegnati;
* incertezze estese;
* coppie di risultati da confrontare.

I diversi scenari vengono selezionati nella fase iniziale dell'applicazione.

## Come sono stati scelti i test statistici

I metodi statistici implementati sono associati alle caratteristiche del confronto da effettuare.

La scelta non dipende quindi esclusivamente dalla disponibilità di una funzione statistica in R, ma dalla struttura dei dati e dall'ipotesi che si vuole verificare.

### Normalità

La verifica della normalità utilizza `stats::shapiro.test()`.

L'implementazione è coerente con **ISO 5479:1997** e consente inoltre di riprodurre l'esempio riportato da Shapiro e Wilk (1965).

### Individuazione di valori anomali

L'individuazione di potenziali valori anomali utilizza il metodo **GESD (Generalized Extreme Studentized Deviate)**.

Il riferimento metodologico è **UNI ISO 16269-4:2019**, §4.3 e Allegato A.

L'applicazione mantiene distinta l'individuazione statistica di un potenziale valore anomalo dalla decisione di escluderlo. Un'osservazione segnalata dal test non viene quindi automaticamente eliminata dall'analisi.

### Confronto delle medie

Per il confronto tra medie viene utilizzato `stats::t.test()`.

Quando le due serie possono presentare varianze differenti, viene utilizzata la formulazione di **Welch**, che non richiede l'assunzione di varianze uguali.

La scelta è particolarmente rilevante nei confronti di risultati analitici, nei quali la variabilità associata ai due insiemi di misure può non essere la stessa.

### Confronto delle varianze

Per il confronto tra due varianze viene utilizzato `stats::var.test()`.

Quando una varianza viene confrontata con un valore assegnato, viene utilizzata la distribuzione chi-quadro secondo l'approccio riportato in **UNI ISO 2854:1988**.

### Confronto mediante En

Quando due risultati sono accompagnati dalle rispettive incertezze estese, il confronto può essere effettuato mediante il parametro **En**.

Il calcolo segue quanto previsto dalla **ISO 13528:2022**, §9.7.

Questo tipo di confronto è diverso da un semplice confronto delle differenze tra valori, perché tiene conto delle incertezze associate ai risultati.

## Riferimenti metodologici

I principali riferimenti utilizzati nello sviluppo sono:

* ISO 5479:1997 — *Statistical interpretation of data — Tests for departure from the normal distribution*;
* UNI ISO 16269-4:2019 — *Interpretazione statistica dei dati — Parte 4: Rilevazione e trattamento dei valori anomali*;
* UNI ISO 2854:1988 — *Interpretazione statistica dei dati — Tecniche di stima e prove di ipotesi relative a medie e varianze*;
* ISO 13528:2022 — *Statistical methods for use in proficiency testing by interlaboratory comparison*;
* Welch, B. L. (1951) — *On the comparison of several mean values: an alternative approach*;
* Zimmerman, D. W. (2004) — *A note on preliminary tests of equality of variances*.

## Controllo del software

SIconfronta è sviluppato come pacchetto R con struttura **golem** e comprende una suite di test automatici.

I test vengono eseguiti automaticamente durante il ciclo di sviluppo attraverso **GitHub Actions**.

Il progetto comprende più di **650 test automatici** e una copertura del codice intorno al **93%**.

I test hanno lo scopo di verificare il comportamento atteso delle funzioni e dell'applicazione e di individuare regressioni durante lo sviluppo e la manutenzione.

**Test automatici e code coverage non equivalgono, da soli, alla validazione formale del software o alla validazione di una procedura analitica.**

L'idoneità di SIconfronta per uno specifico utilizzo deve essere valutata considerando:

* lo scopo dell'analisi;
* la struttura e la qualità dei dati;
* le procedure del laboratorio;
* i requisiti normativi applicabili;
* le eventuali procedure interne di verifica, validazione o qualificazione del software.

Eventuali problemi possono essere segnalati attraverso la sezione [Issues](https://github.com/ARPAL-liguria-it/SIconfronta/issues) del repository.

## Installazione e utilizzo mediante Docker e ShinyProxy

Questa sezione descrive una modalità di installazione utilizzata per distribuire SIconfronta attraverso **Docker** e **ShinyProxy**.

La procedura viene mantenuta nel README anche come documentazione operativa, in modo da poter ricostruire l'ambiente di esecuzione dopo periodi prolungati senza manutenzione del sistema.

### 1. Installare Docker

Installare Docker seguendo le istruzioni relative al proprio sistema operativo.

Per Ubuntu:

https://docs.docker.com/engine/install/ubuntu/

### 2. Preparare ShinyProxy

Creare la directory di lavoro:

```bash id="p5y2s9"
mkdir ~/shinyproxy
cd ~/shinyproxy
```

Scaricare il `Dockerfile` e il file `application.yml` di esempio dal repository:

https://github.com/openanalytics/shinyproxy-config-examples

### 3. Creare la rete Docker

```bash id="q4v7w2"
sudo docker network create sp-example-net
```

### 4. Configurare SIconfronta in ShinyProxy

Nel file `application.yml`, aggiungere SIconfronta alla sezione `specs`:

```yaml id="z2m8x4"
- id: SIconfronta
  container-cmd: ["R", "-e", "SIconfronta::run_app()"]
  container-image: siconfronta:latest
  container-network: sp-example-net
```

### 5. Creare l'immagine di ShinyProxy

Dalla directory `shinyproxy`:

```bash id="r1k6m3"
sudo docker build . -t shinyproxy
```

### 6. Scaricare SIconfronta

Clonare il repository:

```bash id="c7n4p1"
git clone https://github.com/ARPAL-liguria-it/SIconfronta.git
```

### 7. Creare l'immagine Docker di SIconfronta

Dalla directory del repository SIconfronta:

```bash id="v8j3q6"
docker build -f Dockerfile --progress=plain -t siconfronta:latest .
```

### 8. Avviare ShinyProxy

Dalla directory di ShinyProxy:

```bash id="m9f2k5"
docker run --restart=unless-stopped \
  --name shinyproxy \
  -dv /var/run/docker.sock:/var/run/docker.sock:ro \
  --group-add $(getent group docker | cut -d: -f3) \
  --net sp-example-net \
  -p 8080:8080 \
  shinyproxy
```

### 9. Accedere all'applicazione

Aprire:

http://localhost:8080/

Nella configurazione di esempio di ShinyProxy, le credenziali sono:

* **nome utente:** `jack`
* **password:** `password`

Se queste credenziali vengono utilizzate in un ambiente reale, devono essere sostituite con una configurazione di autenticazione appropriata.

### 10. Avviare SIconfronta

Dalla pagina iniziale di ShinyProxy selezionare **SIconfronta** e seguire la procedura guidata dell'applicazione.

### Documentazione ShinyProxy

Per ulteriori informazioni sulla configurazione e sulla distribuzione:

* https://www.shinyproxy.io/documentation/
* https://www.shinyproxy.io/documentation/configuration/

## Stato del progetto

SIconfronta è un'applicazione sviluppata per l'analisi e il confronto di risultati di misura in ambito analitico.

Il repository comprende il codice dell'applicazione, i test automatici e gli strumenti necessari alla costruzione dell'ambiente di distribuzione.

Il software non sostituisce il giudizio tecnico necessario per definire:

* il disegno sperimentale;
* l'adeguatezza dei dati;
* le assunzioni statistiche;
* il significato metrologico dei risultati;
* la conformità a specifiche procedure o requisiti normativi.

Il risultato di un test statistico deve essere interpretato nel contesto del problema analitico per il quale il confronto viene effettuato.

## Licenza

SIconfronta è distribuito secondo i termini della **GNU Affero General Public License v3.0 (AGPL-3.0)**.

## Autore

**Andrea Bazzano**

Repository:

https://github.com/ARPAL-liguria-it/SIconfronta
