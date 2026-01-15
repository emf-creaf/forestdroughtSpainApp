### Mapa 

  1. `Variable`: Selecciona la variable a visualitzar. 

  1. `Data`: Selecciona la data que voleu visualitzar. 
    - Les dates es limiten als darrers 365 dies des de la data més recent disponible. 
    - La data més recent disponible sol ser de fa 5 dies a partir de la data actual, a causa de les limitacions imposades per les fonts de dades (les dades meteorològiques diàries només estan disponibles després de 5 dies). 

  1. `Agregació`: Seleccioneu el nivell d'agregació que voleu visualitzar. 
    - `Cap` mostra una imatge ràster (resolució de 500m). 
    - `Municipis`, `Comarques` i `Províncies` mostren dades agregades pel nivell seleccionat.

### Exploració del mapa

Amb el ratolí es pot modificar la vista del mapa: 

  - Fes servir la roda per augmentar o reduir el zoom del mapa. 
  - Arrossegueu amb el ratolí per moure el mapa. 
  - Arrossegar mentre es manté premuda la tecla `Ctrl` permet canviar l'orientació i l'azimut del mapa. Això és especialment útil en visualitzar polígons de dades agregades.

### Descàrrega de mapes

Els mapes, amb una resolució de 500m, estan disponibles al repositori públic de dades de l'EMF. Són fitxers `gpkg` que contenen punts indicant el centre de cada cel·la.