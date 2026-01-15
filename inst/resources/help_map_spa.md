### Mapa

  1. `Variable`: Selecciona la variable a visualizar.

  1. `Fecha`: Seleccione la fecha que desea visualizar.
    - Las fechas se limitan a los últimos 365 días desde la fecha más reciente disponible.
    - La fecha más reciente disponible suele ser de hace 5 días a partir de la fecha actual, debido a las limitaciones impuestas por las fuentes de datos (los datos meteorológicos diarios solo están disponibles después de 5 días).

  1. `Agregación`: Seleccione el nivel de agregación que desea visualizar.
    - `Ninguno` muestra una imagen ráster (resolución de 500m).
    - `Municipios`, `Comarcas` y `Provincias` muestran datos agregados por el nivel seleccionado.

### Exploración del mapa

Con el ratón se puede modificar la vista del mapa:

  - Usa la rueda para aumentar o reducir el zoom del mapa.
  - Arrastra con el ratón para mover el mapa.
  - Arrastrar mientras se mantiene presionada la tecla `Ctrl` permite cambiar la orientación y el azimut del mapa. Esto es especialmente útil al visualizar polígonos de datos agregados.

### Descarga de mapas

Los mapas, con una resolución de 500m, están disponibles en el repositorio público de datos de la EMF. Son archivos `gpkg` que contienen puntos indicando el centro de cada celda.