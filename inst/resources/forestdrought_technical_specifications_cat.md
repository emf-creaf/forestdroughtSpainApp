---
title: "Especificacions tècniques"
output: html_document
---

### Introducció

**Forestdrought App** proporciona estimacions diàries de fluxos hídrics, estrès i risc d'incendi a les zones forestals de l'Espanya peninsular i les Balears a una resolució de 500m. Els càlculs diaris es realitzen usant les dades climàtiques proporcionades per **Meteoland App** com a input per al model basat en processos MEDFATE, el qual també utilitza informació de la topografia, el sòl i la vegetació de cada cel·la. Com que els càlculs estan basats en un model, poden existir discrepàncies entre els valors predits i els mesuraments en algunes ubicacions.

### Descripció del model MEDFATE

La vegetació es representa en el model com un conjunt de cohorts de plantes de la mateixa espècie i mida similar, i el sòl es representa mitjançant un conjunt de capes verticals discretes amb propietats físiques i hidràuliques conegudes. El model forestal basat en processos realitza càlculs diaris del balanç hídric com a resultat de la interacció entre la precipitació i els diferents processos que causen pèrdues d'aigua (intercepció de la pluja, transpiració de les plantes, evaporació del sòl, escolament i drenatge profund). Els detalls del disseny i la formulació del model es poden trobar en diferents publicacions: [De Cáceres et al. (2015)](https://doi.org/10.1016/j.agrformet.2015.06.012); [(2021)](https://doi.org/10.1016/j.agrformet.2020.108233); [(2023)](https://doi.org/10.5194/gmd-16-3165-2023). El resultat del balanç hídric permet estimar el contingut d'humitat de les fulles i les branques, que al seu torn s'utilitza per estimar la humitat del combustible. El model utilitza l'estructura de la vegetació, la composició i la humitat dels combustibles vius i morts per estimar el risc d'incendi forestal a partir de la simulació local del comportament potencial del foc, utilitzant una adaptació del Sistema de Classificació de les Característiques del Combustible [Prichard et al. (2013)](https://doi.org/10.2737/PNW-GTR-887). El model forestal basat en processos s'implementa al paquet R [medfate](https://emf-creaf.github.io/medfate/) i també es pot trobar documentació detallada en un [llibre de referència](https://emf-creaf.github.io/medfatebook/).

### Parametrització

+ **Celdas objetivo** - La definició de ràster és la mateixa que la de **Meteoland App**. Les celdes objectiu es defineixen com les celdes del ràster que se superposen als polígons del Mapa Forestal Nacional de España ([MFE25]( https://www.miteco.gob.es/ca/biodiversidad/temas/inventarios-nacionales/mapa-forestal-espana/mfe_25.html)).
+ **Topografía** - Las características topográficas (elevación, pendiente y orientación) es deriven d'un model digital d'elevació, originalment a 25m, generat a partir de LiDAR ([PNOA-MDT25](https://centrodedescargas.cnig.es/CentroDescargas/modelo-digital-terreno-mdt25-primera-cobertura)).

+ **Datos de vegetació** - L'estructura de la vegetació i la seva composició en la celda objectiu s'estima inicialment trobant la parcel·la de l'Inventari Forestal Nacional ([Inventarios nacionales]( https://www.miteco.gob.es/ca/biodiversidad/temas/inventarios-nacionales.html)) més propera a l'últim inventari disponible (IFN3 o IFN4) amb espècies dominants i topografía similars. Posteriorment, l'estructura del bosc en la celda objectiu es corregeix utilitzant altres dades disponibles. Específicament, l'alçada dels arbres es corregeix utilitzant un mapa d'altures de cobertes derivades de LiDAR ([Modelo digital de superfícies vegetació](https://centrodedescargas.cnig.es/CentroDescargas/modelo-digital-superficies-vegetacion-mdsnv2_5-primera-cobertura)). La densitat d'arbres també es corregeix, comparant la biomasa aèria total estimada de la celda objectiu amb un mapa de biomases derivades d'observacions àrees i satèl·lit ([Su et al. 2025](https://doi.org/10.5281/zenodo.15032832)). La biomasa foliar es calcula per a cada espècie basant-se en equacions alomètriques (ver **AllometrApp**). L'àrea foliar específica i altres variables específiques de les espècies s'han obtingut com indica en [De Cáceres (2023)](https://doi.org/10.5194/gmd-16-3165-2023).
+ **Sòl** - La textura del sòl, la seva densitat i la quantitat de matèria orgànica van ser obtinguts de la base de dades global [**SoilGrids**](https://files.isric.org/soilgrids/latest/data/) a 250m de resolució i es van usar per calcular propietats hidràuliques del sòl. La profunditat del sòl i el seu contingut en fragments de roca es van modificar usant dades de [Shangguan et al. (2017)](https://doi.org/10.1002/2016MS000686), també a 250m de resolució.

### Càlcul diari

El motor de càlcul de la **Forestdrought App** s'alimenta de les dades meteorològiques processades per la **Meteoland App** (és a dir, descàrrega de dades de les estacions meteorològiques i interpolació sobre cel·les ràster). MEDFATE s'executa a cada cel·la objectiu utilitzant el contingut d'aigua del terra del dia anterior com a estat inicial. El model estima els fluxos daigua que contribueixen al balanç hídric del sòl i actualitza aquest contingut. Posteriorment, s'estima l'estrès hídric de la vegetació, el contingut d'humitat del combustible i les mètriques de risc d'incendi. El contingut d'aigua del sòl final s'emmagatzema per processar-lo l'endemà.

### Resultats

**Forestdrought App** ofereix dades de diferents variables, que poden ser agrupades en categories:

**Humitat del sòl**:

| Variable | Codi | Descripció | Unitats |
|----------|--------|-------------|----------|
| Contingut d'humitat del sòl | `Theta` | Contingut volumètric d'humitat, com la mitjana ponderada de les diferents capes de terra. S'utilitzen com a pesos de la mitjana el volum d'aigua a capacitat de camp | $m^3 \cdot m^{-3}$ |
| Aigua extraïble relativa | `REW` | Aigua disponible a terra, normalitzada entre capacitat de camp [100%] i punt de marciment a - 5MPa [0%] | [%] |
| Potencial hídric del sòl | `Psi` | Potencial hídric mitjana de les diferents capes, calculat com la mitjana ponderada de les diferents capes | $MPa$ |

**Meteorologia** - Les variables meteorològiques més rellevants per al model de balanç hídric són la precipitació (tant en forma líquida com sòlida) i l'evapotranspiració potencial:

| Variable | Codi | Descripció | Unitats |
|----------|-------|---------------------|--------|
| Evapotranspiració potencial | `PET` | Evapotranspiració potencial diària, calculada a partir de l'equació de Penman (1956) | $mm \cdot d^{-1}$ |
| Precipitació | `Precipitation` | Precipitació diària (inclosa la pluja i la neu) | $mm \cdot d^{-1}$ |

**Superfície evaporativa** - La variable de la vegetació més important per al balanç hídric és l'índex d'àrea foliar, ja que determina diversos fluxos. L'índex d'àrea foliar es considera constant en boscos on totes les espècies són perennifòlies. En boscos amb espècies caducifòlies, l'índex d'àrea foliar pot variar segons l'estat fenològic de les espècies.

| Variable | Codi | Descripció | Unitats |
|----------|-------|--------------------|--------|
| Índex dàrea foliar | `LAI` | Àrea foliar de bosc per àrea de terra | $m^2 \cdot m^{-2}$ |

**Balanç hídric** - Les variables següents proporcionen informació sobre els diferents components del balanç hídric diari:

| Variable | Codi | Descripció | Unitats |
|----------|--------|---------------------|---------|
| Intercepció | `Interception` | Pluja interceptada per les fulles i branques de les plantes i evaporada d'aquestes superfícies. El model no inclou les pèrdues per intercepció de la neu. | $mm \cdot d^{-1}$ |
| Evaporació del sòl | `Esòl` | Aigua evaporada de la superfície del sòl. | $mm \cdot d^{-1}$ |
| Aigua líquida exportada | `ELW` | Aigua líquida que surt de la zona forestal com a escorrentia superficial o percolació profunda. | $mm \cdot d^{-1}$ |
| Transpiració de les plantes | `Eplant` | Aigua transportada del sòl a les fulles a través dels teixits vegetals i evaporada a les fulles i tornant a l'atmosfera. | $mm \cdot d^{-1}$ |
| Evapotranspiració real | `AET` | La suma de la intercepció, l'evaporació del sòl i la transpiració de les plantes | $mm \cdot d^{-1}$ |

**Estrès per sequera i risc d'incendi** - Per al mateix contingut d'aigua del sòl, algunes espècies pateixen un estrès per sequera més intens que d'altres. La conductància hidràulica relativa de tota la planta és una mesura de la disminució de la transpiració a causa de l'estrès per sequera. La intensitat de l'estrès per sequera per a cada espècie es defineix com el complement de la conductància relativa de tota la planta. El potencial hídric de la planta s'utilitza per estimar el contingut relatiu d'aigua dels teixits vegetals i, a partir d'aquí, el contingut d'humitat del combustible viu. El contingut d'humitat del combustible mort s'estima seguint [Resco De Dios et al. (2015)](https://doi.org/10.1016/j.agrformet.2015.01.002) i les mètriques de risc d'incendi s'estimen segons [Prichard et al. (2013)](https://doi.org/10.2737/PNW-GTR-887).

| Variable | Codi | Descripció | Unitats |
|----------|-------|--------------------|--------|
| Intensitat de l'estrès per sequera | `DDS` | Mitjana ponderada de l'estrès per sequera entre diferents espècies, utilitzant els valors LAI com a pesos. | [%] |
| Contingut d'humitat del combustible viu | `LFMC` | Contingut d'humitat del combustible fi viu en relació amb el pes sec. Mitjana ponderada de les diferents espècies, utilitzant els valors LAI com a pesos. | [%] |
| Contingut d'humitat del combustible mort | `DFMC` | Contingut d'humitat dels combustibles fins morts en relació amb el pes sec. | [%] |
| Potencial d'incendi superficial | `SFP` | Índex de comportament del potencial d'incendi superficial. | [0-9] |
| Potencial d'incendi de capçades | `CPF` | Índex de comportament del potencial d'incendi de capçades. | [0-9] |