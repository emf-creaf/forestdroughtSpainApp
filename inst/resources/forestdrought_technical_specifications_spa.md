---
title: "Especificaciones técnicas"
output: html_document
---

### Introducción

**Forestdrought App** proporciona estimaciones diarias de flujos hídricos, estrés y riesgo de incendio en las zonas forestales de la España peninsular y Baleares a una resolución de 500m. Los cálculos diarios se realizan usando los datos climáticos proporcionados por **Meteoland App** como input para el modelo basado en procesos MEDFATE, el cual también usa información de la topografía, el suelo y la vegetación de cada celda. Dado que los cálculos están basados en un modelo, pueden existir discrepancias entre los valores predichos y las mediciones en algunas ubicaciones.

### Descripción del modelo MEDFATE

La vegetación se representa en el modelo como un conjunto de cohortes de plantas de la misma especie y tamaño similar, y el suelo se representa mediante un conjunto de capas verticales discretas con propiedades físicas e hidráulicas conocidas. MEDFATE realiza cálculos diarios del balance hídrico como resultado de la interacción entre la precipitación y los diferentes procesos que causan pérdidas de agua (intercepción de lluvia, transpiración vegetal, evaporación del suelo, escorrentía y drenaje profundo). Los detalles del diseño y la formulación del modelo se pueden encontrar en diferentes publicaciones: [De Cáceres et al. (2015)](https://doi.org/10.1016/j.agrformet.2015.06.012); [(2021)](https://doi.org/10.1016/j.agrformet.2020.108233); [(2023)](https://doi.org/10.5194/gmd-16-3165-2023). El resultado del balance hídrico permite estimar el contenido de humedad de hojas y ramas, lo que a su vez se utiliza para estimar la humedad del combustible. El modelo utiliza la estructura, composición y humedad de la vegetación de combustibles vivos y muertos para estimar el riesgo de incendios forestales a partir de la simulación local del comportamiento potencial del fuego, utilizando una adaptación del Sistema de Clasificación de Características de Combustibles [Prichard et al. (2013)](https://doi.org/10.2737/PNW-GTR-887). El modelo forestal basado en procesos se implementa en el paquete R [medfate](https://emf-creaf.github.io/medfate/) y la documentación detallada también se puede encontrar en un [libro de referencia](https://emf-creaf.github.io/medfatebook/).

### Parametrización

+ **Celdas objetivo** - La definición de ráster es la misma que la de **Meteoland App**. Las celdas objetivo se definen como las celdas del ráster que se superponen a los polígonos del Mapa Forestal Nacional de España ([MFE25]( https://www.miteco.gob.es/ca/biodiversidad/temas/inventarios-nacionales/mapa-forestal-espana/mfe_25.html)).
+ **Topografía** - Las características topográficas (elevación, pendiente y orientación) se derivan de un modelo digital de elevación, originalmente a 25m, generado a partir de LiDAR  ([PNOA-MDT25](https://centrodedescargas.cnig.es/CentroDescargas/modelo-digital-terreno-mdt25-primera-cobertura)).
+ **Datos de vegetación** - La estructura de la vegetación y su composición en la celda objetivo se estiman inicialmente encontrando la parcela del Inventario Forestal Nacional ([Inventarios nacionales](	https://www.miteco.gob.es/ca/biodiversidad/temas/inventarios-nacionales.html)) más cercana en el último inventario disponible (IFN3 o IFN4) con especies dominantes y topografía similares. Posteriormente, la estructura del bosque en la celda objetivo se corrige usando otros datos disponibles. Específicamente, la altura de los árboles se corrige usando un mapa de alturas de cubiertas derivado de LiDAR ([Modelo digital de superficies vegetación](https://centrodedescargas.cnig.es/CentroDescargas/modelo-digital-superficies-vegetacion-mdsnv2_5-primera-cobertura)). La densidad de árboles también se corrige, comparando la biomasa aérea total estimada de la celda objetivo con un mapa de biomasas derivado de observaciones aréas y satélite ([Su et al. 2025](https://doi.org/10.5281/zenodo.15032832)). La biomasa foliar se calcula para cada especie basándoes en ecuaciones alométricas (ver **AllometrApp**). El área foliar específica y otras variables específicas de las especies se han obtenido como se explica en [De Cáceres (2023)](https://doi.org/10.5194/gmd-16-3165-2023).
+ **Suelo** - La textura del suelo, su densidad y la cantidad de materia orgánica fueron obtenidos de la base de datos global [**SoilGrids**](https://files.isric.org/soilgrids/latest/data/) a 250m de resolución y se usaron para calcular propiedades hidráulicas del suelo. La profundidad del suelo y su contenido en fragmentos de roca se modificaron usando datos de [Shangguan et al. (2017)](https://doi.org/10.1002/2016MS000686), tambien a 250m de resolución.
+ **Clima** - Las variables meteorológicas se obtuvieron a 500m de **Meteoland App**, interpoladas a partir de los datos de estaciones meteorológicas ofrecidos por [AEMET](https://www.aemet.es/es/portada), el [Servei Meteorlogic de Catalunya](https://www.meteo.cat/), [MeteoGalicia](https://www.meteogalicia.gal/web/home) y la [Red de Información Agroclimática de Andalucia](https://www.juntadeandalucia.es/agriculturaypesca/ifapa/riaweb/web/inicio_estaciones).

### Cálculo diario

El motor de cálculo de la **Forestdrought App** se alimenta de los datos meteorológicos procesados ​​por la **Meteoland App** (es decir, descarga de datos de las estaciones meteorológicas e interpolación sobre celdas ráster). MEDFATE se ejecuta en cada celda objetivo utilizando el contenido de agua del suelo del día anterior como estado inicial. El modelo estima los flujos de agua que contribuyen al balance hídrico del suelo y actualiza dicho contenido. Posteriormente, se estima el estrés hídrico de la vegetación, el contenido de humedad del combustible y las métricas de riesgo de incendio. El contenido de agua del suelo final se almacena para su procesamiento al día siguiente.

### Resultados

**Forestdrought App** ofrece datos de diferentes variables, que pueden ser agrupadas en categorías:

**Humedad del suelo**:

| Variable | Código | Descripción | Unidades |
|----------|--------|-------------|----------|
| Contenido de humedad del suelo | `Theta` | Contenido volumétrico de humedad, como la media ponderada de las diferentes capas de suelo. Se usan como pesos de la media el volumen de agua a capacidad de campo | $m^3 \cdot m^{-3}$ |
| Agua extraíble relativa | `REW` | Agua disponible en el suelo, normalizada entre capacidad de campo [100%] y punto de marchitez a - 5MPa [0%] | [%] |
| Potencial hídrico del suelo | `Psi` | Potencial hídrico promedio de las diferentes capas, calculado como la media ponderada | $MPa$ |

**Meteorología** - Las variables meteorológicas más relevantes para el modelo de balance hídrico son la precipitación (tanto líquida como sólida) y la evapotranspiración potencial:

| Variable | Código | Descripción | Unidades |
|----------|-------|--------------------|--------|
| Evapotranspiración potencial | `PET` | Evapotranspiración potencial diaria, calculada a partir de la ecuación de Penman (1956) | $mm \cdot d^{-1}$ |
| Precipitación | `Precipitation` | Precipitación diaria (incluyendo lluvia y nieve) | $mm \cdot d^{-1}$ |

**Superficie evaporativa** - La variable de la vegetación más importante para el balance hídrico es el índice de área foliar, ya que determina varios flujos. El índice de área foliar se considera constante en bosques donde todas las especies son perennifolias. En bosques con especies caducifolias, el índice de área foliar puede variar según el estado fenológico de las especies.

| Variable | Código | Descripción | Unidades |
|----------|-------|--------------------|--------|
| Índice de área foliar | `LAI` | Área foliar de bosque por área de suelo | $m^2 \cdot m^{-2}$ |

**Balance hídrico** - Las siguientes variables proporcionan información sobre los diferentes componentes del balance hídrico diario:

| Variable | Código | Descripción | Unidades |
|----------|--------|--------------------|--------|
| Intercepción | `Interception` | Lluvia interceptada por las hojas y ramas de las plantas y evaporada de estas superficies. El modelo no incluye las pérdidas por intercepción causadas por la nieve. | $mm \cdot d^{-1}$ |
| Evaporación del suelo | `Esoil` | Agua evaporada de la superficie del suelo. | $mm \cdot d^{-1}$ |
| Agua líquida exportada | `ELW` | Agua líquida que sale de la parcela forestal como escorrentía superficial o percolación profunda. | $mm \cdot d^{-1}$ |
| Transpiración de las plantas | `Eplant` | Agua transportada del suelo a las hojas a través de los tejidos vegetales, evaporada en ellas y devuelta a la atmósfera. | $mm \cdot d^{-1}$ |
| Evapotranspiración real | `AET` | La suma de la intercepción, la evaporación del suelo y la transpiración de las plantas | $mm \cdot d^{-1}$ |

**Estrés por sequía y riesgo de incendio** - Para el mismo contenido de agua en el suelo, algunas especies sufren un estrés por sequía más intenso que otras. La conductancia hidráulica relativa de toda la planta mide la disminución de la transpiración debido al estrés por sequía. La intensidad del estrés por sequía para cada especie se define como el complemento de la conductancia relativa de toda la planta. El potencial hídrico de la planta se utiliza para estimar el contenido relativo de agua de los tejidos vegetales y, a partir de ahí, el contenido de humedad del combustible vivo. El contenido de humedad del combustible muerto se estima según [Resco De Dios et al. (2015)](https://doi.org/10.1016/j.agrformet.2015.01.002) y las métricas de riesgo de incendio se estiman según [Prichard et al. (2013)](https://doi.org/10.2737/PNW-GTR-887).

| Variable | Código | Descripción | Unidades |
|----------|-------|--------------------|--------|
| Intensidad del estrés por sequía | `DDS` | Media ponderada del estrés hídrico en diferentes especies, utilizando el IAF como ponderación. | [%] |
| Contenido de humedad del combustible vivo | `LFMC` | Contenido de humedad del combustible fino vivo en relación con el peso seco. Promedio ponderado de las diferentes especies, utilizando los valores del IAF como ponderación. | [%] |
| Contenido de humedad del combustible muerto | `DFMC` | Contenido de humedad del combustible fino muerto en relación con el peso seco. | [%] |
| Potencial de incendio superficial | `SFP` | Índice de comportamiento del potencial de incendio superficial. | [0-9] |
| Potencial de incendio de copa | `CPF` | Índice de comportamiento del potencial de incendio de copa. | [0-9] |