extensions [rnd]

;; Each potential solution is represented by a turtle.
turtles-own [
  bits           ;; list of 0's and 1's
  fitness
]

globals [
  winner         ;; turtle that currently has the best solution
]

to setup
  clear-all
  create-turtles population-size [
    set bits n-values world-width [one-of [0 0 0 0 1]]
    calculate-fitness
    hide-turtle  ;; the turtles' locations are not used, so hide them
  ]
  update-display
  reset-ticks
end

to go
  if [fitness] of winner = world-width
    [ stop ]
  ifelse model-type? = "Generacional"
    ; Generational GA Schema
    [next-generation]
  ; Stationary GA Schema
  [next-station]

  update-display
  tick
end

to update-display
  set winner max-one-of turtles [fitness]
  ask patches [
    ifelse item pxcor ([bits] of winner) = 1
      [ set pcolor white ]
      [ set pcolor black ]
  ]
end

;; ===== Generating Solutions

;; Each solution has its "fitness score" calculated.
;; Higher scores mean "more fit", and lower scores mean "less fit".
;; The higher a fitness score, the more likely that this solution
;;   will be chosen to reproduce and create offspring solutions
;;   in the next generation.
;;
to calculate-fitness       ;; turtle procedure
  ;; For the "ALL-ONES" problem, the fitness is simply equal to the number of ones
  ;; that occur in this solution's bits.
  ;; However, you could solve more interesting problems by changing this procedure
  ;; to evaluate the bits in other ways.  For instance, the bits might
  ;; encode rules for how a turtle should move across the world in a search for food.
  set fitness length (remove 0 bits)
end


;; This procedure does the main work of the genetic algorithm.
;; We start with the old generation of solutions.
;; We choose solutions with good fitness to produce offspring
;; through crossover (sexual recombination), and to be cloned
;; (asexual reproduction) into the next generation.
;; There is also a chance of mutation occurring in each individual.
;; After a full new generation of solutions has been created,
;; the old generation dies.

to next-generation
  ; The following line of code looks a bit odd, so we'll explain it.
  ; if we simply wrote "LET OLD-GENERATION TURTLES",
  ; then OLD-GENERATION would mean the set of all turtles, and when
  ; new solutions were created, they would be added to the breed, and
  ; OLD-GENERATION would also grow.  Since we don't want it to grow,
  ; we instead write "TURTLES WITH [TRUE]", which makes OLD-GENERATION
  ; an agentset, which doesn't get updated when new solutions are created.
  let old-generation turtles with [true]

  ; Some number of the population is created by crossover each generation
  ; we divide by 2 because each time through the loop we create two children.
  let crossover-count  (floor (population-size * crossover-rate / 100 / 2))

  repeat crossover-count
  [
    let parents []
    set parents selection old-generation

    let child-bits []
    ifelse crossover-type? = "Cruce Uniforme"
      [set child-bits uniform-crossover ([bits] of (item 0 parents)) ([bits] of (item 1 parents))]
    [set child-bits segment-crossover ([bits] of (item 0 parents)) ([bits] of (item 1 parents))]

    ; create the two children, with their new genetic material
    ask item 0 parents [ hatch 1 [ set bits item 0 child-bits ] ]
    ask item 1 parents [ hatch 1 [ set bits item 1 child-bits ] ]
  ]

  ; the remainder of the population is created by cloning
  ; selected members of the previous generation
  repeat (population-size - crossover-count * 2)
  [
    ask max-one-of (n-of 3 old-generation) [fitness]
      [ hatch 1 ]
  ]

  ; Include the best solution of old-generation at random position
  if elitism? [let best-one max-one-of old-generation [fitness]
                ask one-of turtles [set bits ([bits] of best-one) set fitness ([fitness] of best-one)]]

  ask old-generation [ die ]

  ; now we're just talking to the new generation of solutions here
  ask turtles
  [
    ; there's a chance of mutations occurring
    if mutation? [mutate]
    ; finally we update the fitness value for this solution
    calculate-fitness
  ]
end


;; Procedure to modify the actual generation of solutions.
;; We choose solutions with good fitness to produce offspring
;; through crossover (sexual recombination) and replacing
;; the worst solution of the actual generation with the best child solution.
;; It coult take more ticks to converge than generational model.

to next-station

  repeat replacement-number
  [
    let population turtles with [true]

    let parents []
    set parents selection population

    let child-bits []
    ifelse crossover-type? = "Cruce Uniforme"
      [set child-bits uniform-crossover ([bits] of (item 0 parents)) ([bits] of (item 1 parents))]
    [set child-bits segment-crossover ([bits] of (item 0 parents)) ([bits] of (item 1 parents))]

    ; create the two children, with their new genetic material
    ask item 0 parents [ hatch 1 [ set bits item 0 child-bits ] ]
    ask item 1 parents [ hatch 1 [ set bits item 1 child-bits ] ]

    ; We set the 'Who' label of the new solutions
    let child1 max ([who] of turtles)
    let child2 child1 - 1

    ; Fitness of the generated solutions
    ask turtle child1 [calculate-fitness]
    ask turtle child2 [calculate-fitness]

    replacement population child1 child2
  ]

  ask turtles
  [
    ; there's a chance of mutations occurring
    ; finally we update the fitness value for this solution
    if mutation? [mutate calculate-fitness]
  ]

End

;; Selection operator
to-report selection [population]

  ifelse selection-type? = "Torneo"
    ;; Tournament selection
    [
      ; We use "tournament selection", with tournament size = 3 by default
      ; This means, we randomly pick 3 solutions from the previous generation
      ; and select the best one of those 3 to reproduce.
      let parent1 max-one-of (n-of n-tournament-selection population) [fitness]
      let parent2 max-one-of (n-of n-tournament-selection population) [fitness]
      report list parent1 parent2
    ]
  ;; Roulette selection
  [
    let parent1 rnd:weighted-one-of population [fitness]
    let parent2 rnd:weighted-one-of population [fitness]
    report list parent1 parent2

    ;; Roulette wheel selection in NetLogo
    ;; https://ccl.northwestern.edu/netlogo/docs/rnd.html
  ]
End


;; Replacement operator
;; New child solutions have to compete to replace the worst solution

to replacement [population child1 child2]

  let f-child1 [fitness] of turtle child1
  let f-child2 [fitness] of turtle child2

  ; Worst solution
  let worst-solution min-one-of (population) [fitness]

  ; The best child replaces the worst solution
  ifelse f-child1 > f-child2
    [ask worst-solution [set bits [bits] of turtle child1 set fitness f-child1]]
  [ask worst-solution [set bits [bits] of turtle child2 set fitness f-child2]]

  ask turtle child1 [ die ]
  ask turtle child2 [ die ]

End


;; ===== Crossover operators

;; This reporter performs one-point crossover on two lists of bits.
;; That is, it chooses a random location for a splitting point.
;; Then it reports two new lists, using that splitting point,
;; by combining the first part of bits1 with the second part of bits2
;; and the first part of bits2 with the second part of bits1;
;; it puts together the first part of one list with the second part of
;; the other.

;; Uniforme en un punto

to-report uniform-crossover [bits1 bits2]
  let split-point 1 + random (length bits1 - 1)
  report list (sentence (sublist bits1 0 split-point)
                        (sublist bits2 split-point length bits2))
              (sentence (sublist bits2 0 split-point)
                        (sublist bits1 split-point length bits1))
end


;; This reporter performs one-segment crossover on one list of bits.
;; It chooses a random location and a random size for creating a segment.
;; Then it reports a new list, we have to use this crossover two times to generate
;; 2 new solutions.

to-report segment-crossover [bits1 bits2]

  let childs []

  ; We iterate 2 times to generate 2 childs
  repeat 2
  [
    ; Parameters to create the segment
    let n-gens (length bits1)
    let begin-segment random n-gens
    let tam-segment random n-gens

    let child n-values n-gens [0]

    ; Genetic information (segment) from father1
    let i 0
    while [i < tam-segment]
      [
        let modify-gen ((begin-segment + i) mod n-gens)
        set child replace-item modify-gen child (item modify-gen bits1)
        set i i + 1
      ]

    ; Genetic information (rest of gens) from father2
    let rest (n-gens - tam-segment)
    set i ((begin-segment + tam-segment) mod n-gens)

    while [i < (((begin-segment + tam-segment) mod n-gens) + rest)]
      [
        set child replace-item (i mod n-gens) child item (i mod n-gens) bits2
        set i i + 1
      ]

    ; Insert to last position of list
    set childs lput child childs
  ]

  report childs
end


;; ===== Mutation operator

;; This procedure causes random mutations to occur in a solution's bits.
;; The probability that each bit will be flipped is controlled by the
;; MUTATION-RATE slider.
to mutate   ;; turtle procedure
  set bits map [ b ->
    ifelse-value random-float 100.0 < mutation-rate
      [ 1 - b ]
      [ b ]
  ] bits
end

;; ===== Diversity Measures

;; Our diversity measure is the mean of all-pairs Hamming distances between
;; the genomes in the population.
to-report diversity
  let distances []
  ask turtles [
    let bits1 bits
    ask turtles with [self > myself] [
      set distances fput (hamming-distance bits bits1) distances
    ]
  ]
  ; The following  formula calculates how much 'disagreement' between genomes
  ; there could possibly be, for the current population size.
  ; This formula may not be immediately obvious, so here's a sketch of where
  ; it comes from.  Imagine a population of N turtles, where N is even, and each
  ; turtle has  only a single bit (0 or 1).  The most diverse this population
  ; can be is if half the turtles have 0 and half have 1 (you can prove this
  ; using calculus!). In this case, there are (N / 2) * (N / 2) pairs of bits
  ; that differ.  Showing that essentially the same formula (rounded down by
  ; the floor function) works when N is odd, is left as an exercise to the reader.
  let max-possible-distance-sum floor (count turtles * count turtles / 4)

  ; Now, using that number, we can normalize our diversity measure to be
  ; between 0 (completely homogeneous population) and 1 (maximally heterogeneous)
  report (sum distances) / max-possible-distance-sum
end

;; The Hamming distance between two bit sequences is the fraction
;; of positions at which the two sequences have different values.
;; We use MAP to run down the lists comparing for equality, then
;; we use LENGTH and REMOVE to count the number of inequalities.
to-report hamming-distance [bits1 bits2]
  report (length remove true (map [ [b1 b2] -> b1 = b2 ] bits1 bits2)) / world-width
end


; Copyright 2008 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
76
52
884
85
-1
-1
8.0
1
10
1
1
1
0
1
1
1
0
99
0
2
1
1
1
ticks
30.0

BUTTON
203
165
288
198
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
115
125
288
158
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
100
229
300
262
population-size
population-size
5
200
150.0
5
1
NIL
HORIZONTAL

PLOT
369
119
788
341
Fitness Plot
gen #
raw fitness
0.0
20.0
0.0
101.0
true
true
"" ""
PENS
"best" 1.0 0 -2674135 true "" "plot max [fitness] of turtles"
"avg" 1.0 0 -10899396 true "" "plot mean [fitness] of turtles"
"worst" 1.0 0 -13345367 true "" "plot min [fitness] of turtles"

BUTTON
115
165
200
198
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
79
551
211
584
mutation-rate
mutation-rate
0
10
0.2
0.1
1
NIL
HORIZONTAL

PLOT
370
380
790
558
Diversity Plot
gen #
diversity
0.0
20.0
0.0
1.0
true
false
"" ""
PENS
"diversity" 1.0 1 -8630108 true "" "if plot-diversity? [ plot diversity ]"

SWITCH
497
585
669
618
plot-diversity?
plot-diversity?
0
1
-1000

SLIDER
836
321
1033
354
crossover-rate
crossover-rate
0
100
80.0
1
1
NIL
HORIZONTAL

TEXTBOX
836
298
986
316
Modelo Generacional
12
0.0
1

TEXTBOX
837
442
987
460
Modelo Estacionario
12
0.0
1

SLIDER
836
466
1034
499
replacement-number
replacement-number
0
population-size
14.0
1
1
NIL
HORIZONTAL

CHOOSER
833
214
1031
259
model-type?
model-type?
"Generacional" "Estacionario"
0

SWITCH
221
551
331
584
mutation?
mutation?
0
1
-1000

TEXTBOX
453
24
603
42
Mejor cromosoma
12
0.0
1

MONITOR
910
45
970
90
Fitness
[fitness] of winner
0
1
11

SLIDER
90
467
316
500
n-tournament-selection
n-tournament-selection
2
population-size / 2
3.0
1
1
NIL
HORIZONTAL

CHOOSER
122
311
281
356
crossover-type?
crossover-type?
"Cruce Uniforme" "Segmento Fijo"
0

TEXTBOX
153
286
303
304
Tipo de cruce
12
0.0
1

TEXTBOX
143
380
293
398
Tipo de selección
12
0.0
1

CHOOSER
130
405
268
450
selection-type?
selection-type?
"Torneo" "Ruleta"
0

SWITCH
837
376
956
409
elitism?
elitism?
0
1
-1000

TEXTBOX
172
523
322
541
Mutación
12
0.0
1

TEXTBOX
832
183
982
201
Tipo de modelo
12
0.0
1

@#$#@#$#@
## ¿QUÉ ES?

Este modelo demuestra el uso de los algoritmo genético en un problema muy simple, encontrar una el cromosoma que tiene todos los genes con valor 1. 

Los algoritmos genéticos (GA) son una técnica informática con inspiración biológica que combina nociones de la genética mendeliana y la evolución darwiniana para buscar buenas soluciones a los problemas (incluidos los problemas difíciles o NP duros). El GA funciona generando una población aleatoria de soluciones a un problema, evaluando esas soluciones y luego usando clonación, recombinación y mutación para crear nuevas soluciones al problema.

En este modelo usamos el problema simple de "Todos unos" o mśa conocido del ingles "ALL 1" para demostrar cómo esto es posible. Usamos un problema tan simple en este modelo para resaltar solo la técnica de búsqueda de la solución. La idea del problema es encontrar una cadena de bits (es decir, una secuencia de solo unos y ceros) que contenga todos unos y no ceros. Por tanto, la cadena que mejor resuelve este problema es "111111 ... 111".


## CÓMO FUNCIONA

El algoritmo genético se compone de los siguientes pasos.

1) Se crea una población de soluciones aleatorias. Cada solución consta de una serie de "1" y "0" mezclados aleatoriamente.

2) Cada solución se evalúa sobre la base de qué tan bien resuelve el problema. Esta medida de la "bondad" de la solución se llama su "adecuación". En este modelo, nuestro objetivo es simplemente encontrar una solución que consista en todos los "1". (En las aplicaciones del algoritmo genético en el mundo real, los objetivos son mucho más complejos, pero las soluciones todavía suelen codificarse como cadenas binarias).

3) Hay dos variantes de los modelos para crear la nueva generación de soluciones:

- ** Modelo generacional **: se crea una nueva generación de soluciones a partir de la generación anterior, en la que las soluciones que tienen puntuaciones de aptitud más altas (mayor fitness) tienen más probabilidad de ser elegidas como soluciones "principales" que aquellas que tienen puntuaciones de aptitud bajas.

- ** Modelo estacionario **: Se eligen y combinan dos padres de la población para crear un par de hijos. El mejor de ellos reemplazará una solución de la población real (algunos individuos se reemplazan en cada estado).


A) Hay dos métodos de selección en este modelo:

- ** Selección por torneo **: con un tamaño de torneo de 3 por defecto.
Esto significa que se extraen aleatoriamente 3 soluciones de la generación anterior y se elige la que tiene la mayor aptitud para convertirse en padre.

- ** Selección por ruleta **: también conocida como selección de rueda de ruleta. El nivel de fitness se utiliza para asociar una probabilidad de selección con cada cromosoma.

B) Se elige a uno o dos padres para crear hijos. Con uno de los padres, el hijo es un clon o copia del padre. Con dos padres, el proceso es el análogo digital de la recombinación sexual: los dos hijos heredan parte de su material genético de uno de los padres y parte del otro. Hay dos variantes:

- ** Cruce uniforme **: se selecciona un corte (punto) en el primer padre o cromosoma. Todos los datos más allá de este punto se intercambiarán entre los dos padres.

- ** Cruce por segmento **: también conocido como cruce de doble punto (DPX). En lugar de cortar los cromosomas parentales en un solo punto como en el caso anterior, se eligen dos cortes al azar.

C) También existe la posibilidad de que ocurra una mutación, y algunos de los bits del niño cambiarán de "1" a "0", o viceversa.

4) Se repiten los pasos 2 y 3 anteriores hasta que se encuentre una solución que resuelva con éxito el problema.


## CÓMO USARLO

Presione el botón SETUP para crear una población inicial aleatoria de soluciones.

Presione el botón PASO para crear una nueva generación a partir de la generación anterior.

Presione el botón GO para que se ejecute el algoritmo genético hasta que se encuentre una solución.

La mejor solución encontrada en cada generación se muestra en la visualización del modelo. Cada columna blanca representa un bit "1" y cada columna negra representa un bit "0".


=== Parámetros ===

El deslizador POPULATION-SIZE controla el número de soluciones o cromosomas que están presentes en cada generación.

El deslizador CROSSOVER-RATE controla qué porcentaje de cada nueva generación se crea a través de la reproducción sexual (recombinación o cruce entre el material genético de dos padres) y qué porcentaje (100 - CROSSOVER-RATE) se crea a través de la reproducción asexual (clonación de la genética de uno de los padres). material).

El deslizador MUTATION-RATE controla el porcentaje de probabilidad de mutación. Esta posibilidad se aplica a cada posición en la cadena de bits de un nuevo individuo. Por ejemplo, si la cadena tiene 100 bits de longitud y la tasa de mutación se establece en 1%, entonces, en promedio, se cambiará un bit durante la creación de cada nuevo individuo.

El interruptor PLOT-DIVERSITY? controla si queremos dibujar o no la cantidad de diversidad dentro de la población de soluciones en cada generación. Se muestra en el "Gráfico de diversidad". Si no está activado el interruptor PLOT-DIVERSITY? aumenta significativamente la velocidad del modelo porque calcular la diversidad requiere mucho bastante cómputo.

El selector CROSSOVER-TYPE? controla qué tipo de cruce se va a utilizar el algoritmo genético: cruce uniforme o cruce por segmento.

El selector SELECTION-TYPE? controla qué tipo de método de selección preferimos para seleccionar a los padres: selección por torneo o selección por ruleta.

El deslizador N-TOURNAMENT-SELECTION controla cuantos posibles padres vas a estar involucrados cuando se haya seleccionado la selección por torneo. Normalmente se utiliza un valor de 3. 

El selector MODEL-TYPE? controla qué esquema (forma de generar la siguiente población) será utilizada por nuestro algoritmo genético.

El interruptor ELITISM? controla si queremos garantizar que la mejor solución encontrada (el cromosoma con el mejor valor de fitness) se mantenga en la siguiente generación o no.

El deslizador REPLACEMENT-NUMBER controla cuanto pares de cromosomas queremos que sean reemplazados antes de concluir con una estación y pasasr a la siguiente generación. Si tenemos un valor de 10 para esta variable, 20 nuevos individuos serán insertados en la generación actual para generar una generación nueva.

El gráfico "Fitness Plot" es usado para mostrar al mejor, peor y promedio de los valors de fitness de las soluciones en cada una de las generaciones.


## CREDITOS

Este modelo forma parte del proyecto fin de grado (TFG) de Jose A. Martín Melguizo, dirigido por Rocio Romero Zaliz en la Universidad de Granada (UGR)

Granada, 9 de marzo de 2021

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/4.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

![CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/3.0/88x31.png)


## REFERENCIAS

Este modelo está basado en el trabajo de John H. Holland, que es considerado como el padre de los algoritmos genéticos.  Véase en el libro: "Adaptation in Natural and Artificial Systems", 1992, MIT Press.

Este modelo es una extensión de:

* Stonedahl, F. and Wilensky, U. (2008).  NetLogo Simple Genetic Algorithm model.  http://ccl.northwestern.edu/netlogo/models/SimpleGeneticAlgorithm.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.


@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
need-to-manually-make-preview-for-this-model
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
