;; on crée une population d'agents
breed [population agent]

;; chaque agent a un état "authority" (et d'autres attributs implicites)
population-own [authority converts]

globals [winner? nbs started?]

to startup
print "start !"
ca
set started? false
end

;;======================================================================
to setup
  ca
  set-default-shape population "person"
  create-custom-population nb-of-agents
    [
      ifelse (random 100 < chefs)
      ;; chefs are red, other green
        [set authority 1 set color red]
        [set authority 0 set color green]
      setxy random nb-of-plants (- who)
      facexy xcor + 1 (- who)
      set converts 0
    ]
ask patches with [pxcor < nb-of-plants and pycor > (- nb-of-agents)] [set pcolor white]
setup-plot
print "setup !"
set started? true
end

;;======================================================================
to forget
let place nobody
if (forget?) [
     ask population [
       if (random 1000 < forget-it)
         [set place one-of patches with [pycor = pycor-of myself and not empty? plabel]
          if (is-patch? place) 
             [set plabel ""
             set pcolor white]]
           ]
       ]
end

;;======================================================================
to move
ask population
  [
 ifelse (random-walk?)
         [setxy random nb-of-plants (- who)] 
         [if (random 100 < mobility)
             [ifelse ( xcor = nb-of-plants - 1) [set xcor 0] [set xcor xcor + 1]]]
  ]
end

;;======================================================================
to name
ask population with [empty? plabel]
  [
   ;; no symbol attached yet to this place
       let symbol ""
       let mate nobody
       if (social?) 
       ;; agent will ask to any mate here for the name of the place, or decide one if no mate is available
       ;;!!!!!!!!!!!!!!!!!!!!!!!!!!!
       ;; la règle de socialité complète n'est pas appliquée ici - voir à copier "influence"
       ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         [
           set mate one-of population with [pxcor = pxcor-of myself and self != myself and not empty? plabel]
           if (mate != nobody)
             [set symbol plabel-of mate]
         ]
         
         if (empty? symbol) [
           set symbol word "" (random maxword + 1)]

         set plabel check-unique symbol 
     ]
end

;;======================================================================
to influence
ask population with [not empty? plabel]
;; only for those having a word to discuss
  [
   let mate nobody
   
   if (influencer = "one random") [ 
     set mate one-of population with [xcor-of myself = xcor-of self and self != myself and  not empty? plabel]]
   ;;; choose one among those "visiting" the same object at this time


   if (influencer = "chef") [ 
     set mate one-of population with [authority = 1 and xcor-of myself = xcor-of self and self != myself and not empty? plabel]]
   ;;; choose one chef among those "visiting" the same object at this time
   
   if (influencer = "leader") [ 
     set mate max-one-of population with [xcor-of myself = xcor-of self and self != myself and not empty? plabel] [converts]]
   ;;; choose one among the most influencing "visiting" the same object at this time
   
    if (influencer = "mode") [
    user-message "Not implemented (yet !)"
    ;; very heavy and expensive procedure !!!
;      let votes n-values (maxword + 1) [0]
;      let symbol 0
;      ask population with [xcor-of myself = xcor-of self and self != myself and not empty? plabel]
;        [set symbol (read-from-string plabel)
;         set votes replace-item symbol votes (item symbol votes)
;        ]
;      let nbmode max votes
;      let mode position (one-of filter [? = nbmode] votes) votes
;      set mate one-of population with [xcor-of myself = xcor-of self and self != myself and plabel = word "" mode]
    ]
   
   if (mate != nobody) [
       ifelse (authority = authority-of mate and converts = converts-of mate) [
       ;; same authority, same influence
           ifelse( random 100 < convincing) [
             convince mate
             ][
             if (random 100 < (100 - convincing))
               [trust mate]
             ]
         ][ 
           set winner? (authority > authority-of mate or converts > converts-of mate)
           ifelse (winner?)
                 [convince mate]
                 [trust mate]
         ]
         
     ] 
  ]
end

;;======================================================================
to trust [mate]
   set plabel check-unique plabel-of mate
   if (converts = converts-of mate) [
      set converts-of mate converts-of mate + 1
      set converts converts - 1]
end

;;======================================================================
to convince [mate]
   ask mate [set plabel check-unique plabel-of myself]
   if (converts = converts-of mate) [
       set converts-of mate converts-of mate - 1
       set converts converts + 1]
end

;;======================================================================
to-report check-unique [symbol]
 ;; check unicity of the symbol           
 ifelse (unique? and member? symbol values-from patches with [pycor-of self = pycor-of myself and pxcor-of self != pxcor-of myself and not empty? plabel] [plabel])
        [report ""]
        [report symbol]
end

;;======================================================================
to go
  if (not started?) [
    user-message "Please setup first with parameters 1-4 and button setup !!!"
    stop]
  no-display
  forget
  move
  name
  influence
  ask patches with [pxcor < nb-of-plants and pycor > (- nb-of-agents)]
      [set pcolor ifelse-value (empty? plabel) [white] [read-from-string plabel]]
  display
  plot-results
end

;;======================================================================
to plot-results
set nbs []
let id 0
while [id < nb-of-plants]
  [
    set nbs lput length remove-duplicates values-from patches with [pxcor = id and not empty? plabel] [read-from-string plabel] nbs
    set id id + 1
  ]
clear-output
output-write nbs output-print ""
set nbs sort nbs

;;set nbs sort nbs
set-current-plot "conviction"
set-current-plot-pen "min"
plot min values-from population [converts]
set-current-plot-pen "max"
plot max values-from population [converts]
set-current-plot-pen "mean"
plot mean values-from population [converts]
;;plot mean

set-current-plot "histo"
set-plot-x-range 0 max nbs
histogram-list nbs

set-current-plot "histotal"
histogram-list values-from patches with [not empty? plabel and pxcor < nb-of-plants and pycor > (- nb-of-agents)] [read-from-string plabel]

set-current-plot "distrib"
set-current-plot-pen "min"
plot min nbs
set-current-plot-pen "max"
plot max nbs
set-current-plot-pen "mean"
plot mean nbs
end

;;======================================================================
to setup-plot
  set-current-plot "histo"
  set-plot-y-range 0 30
;;  set-histogram-num-bars max-pxcor + 1
  set-plot-pen-interval 1
  set-plot-pen-mode 1
  set-current-plot "histotal"
  set-plot-x-range 1 maxword
  set-plot-pen-interval 1
  set-plot-pen-mode 1
end
@#$#@#$#@
GRAPHICS-WINDOW
584
10
1094
541
-1
-1
10.0
1
8
1
1
1
0
1
0
1
0
49
-49
0

CC-WINDOW
5
555
1103
650
Command Center
0

BUTTON
21
158
84
191
NIL
setup
NIL
1
T
OBSERVER
T
NIL

BUTTON
428
12
491
45
NIL
go
T
1
T
OBSERVER
T
NIL

SLIDER
19
83
162
116
maxword
maxword
1
250
81
1
1
(max 250)

SLIDER
20
120
163
153
chefs
chefs
0
100
35
1
1
/100

PLOT
5
198
411
337
distrib
NIL
NIL
0.0
10.0
0.0
10.0
true
true
PENS
"mean" 1.0 0 -16777216 true
"max" 1.0 0 -2674135 true
"min" 1.0 0 -10899396 true

PLOT
8
377
219
534
histo
Nbs of words per object
Nbs of Objects
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 1 -16777216 true

PLOT
222
383
382
519
conviction
NIL
NIL
0.0
10.0
0.0
10.0
true
false
PENS
"min" 1.0 0 -10899396 true
"max" 1.0 0 -2674135 true
"mean" 1.0 0 -13345367 true

SLIDER
324
55
496
88
mobility
mobility
0
100
100
1
1
/100

SWITCH
197
162
300
195
forget?
forget?
1
1
-1000

SLIDER
300
162
441
195
forget-it
forget-it
0
100
10
1
1
/1000

SWITCH
195
55
327
88
random-walk?
random-walk?
0
1
-1000

SWITCH
195
89
298
122
unique?
unique?
0
1
-1000

SLIDER
441
127
566
160
convincing
convincing
0
100
15
1
1
/100

SWITCH
195
124
298
157
social?
social?
0
1
-1000

OUTPUT
3
333
547
381

CHOOSER
302
114
441
160
influencer
influencer
"one random" "mode" "chef" "leader"
0

SLIDER
19
10
162
43
nb-of-plants
nb-of-plants
1
50
50
1
1
(max 50)

SLIDER
19
47
162
80
nb-of-agents
nb-of-agents
1
50
50
1
1
(max 50)

TEXTBOX
1
16
16
34
1.

TEXTBOX
3
56
18
74
2.

TEXTBOX
3
93
18
111
3.

TEXTBOX
3
127
18
145
4.

TEXTBOX
5
167
20
185
5.

TEXTBOX
169
10
406
54
ROBOTANIC GARDEN\nTo use it, please follow the numbers...\nTo start and stop use the "Go" button.

TEXTBOX
176
64
191
82
6.

TEXTBOX
174
99
189
117
7.

TEXTBOX
176
135
191
153
8.

TEXTBOX
174
168
189
186
9.

TEXTBOX
404
21
424
39
10.

PLOT
384
381
584
531
histotal
NIL
NIL
0.0
10.0
0.0
10.0
true
false

@#$#@#$#@
***   ROBOTANIC GARDEN   ***
-----------

DEMO CASE
-----------
A botanic garden is organized as a ring of plants, but all labels have been stolen !
A group of scholars roams around and try to name the plants on some lists they hold. When they meet each other they can discuss the names. Some of them are obviously elder, which gives them some a priori authority - we call them "chefs".
They can show each other their current list and how many they have named and how often thay changed the name. Thus, the most stable the list, the most trustable the scholar - this is influence.

OBJECTIVE
-----------
Model the dynamics of social binding of sumbols on objects based on a simple authority and influence model.

HOW IT WORKS
------------
Agents are sharing a common space of objects that can be referenced by words (symbols).
Here a column of the space represents an object (a plant). The agents stay on their given lines and move horizontally. Thereby they explore the world and can discover some objects. They "meet" when they are on the same column. 

When an object is discovered the agent can attach a word to it, represented simply by a numeric value.

When they meet various rules are applied to decide the name of the plant. See below for details.

HOW TO USE IT

Parameters and buttons are numbered to help choosing them in order.
Choose 1. the number of plants and 2. the number of scholars.
Then choose 3. the number of available words, and choose 4. the hierarchical distribution ("chefs"). then 5. "setup" the project. 

You can then directly run it using the 10. "go" button. to stop press it again.
Observe the map of the agents roaming to name the plants.
Monitor the distribution of number of names for each plant (min, mean, max), the histogram, and the raw values per column. You can also see ("histotal") the overall distribution of names used.

Meanwhile all parameters 3 and 6 to 9 can be changed.

RUNTIME PARAMETERS

6. When "random walk" is activated, the mobility trigger is idle. Agents jump randomly form one place to another. When "random walk" is off, agents move one step per round, with a probability "mobility"

7. When "unique" is activated, agents check that the word they selected or obtained by interaction is unique in their current list, if not they resign naming this place.

8. If "social" is activated, agents discuss to get a name when none exists here.
Possible modes are:
	- "one random" : any mate present locally can influence self
	- "chef" : only local chefs can influence me
	- "leader" : only local leaders can influence me	
	- "mode" : *** not verified code *** should simulate voting among local agents

9. In the "forget" mode, agents can forget randomly some names at each round.

During runtime, you can increase or decrease the number of available words.

FUTURE IMPROVEMENTS

- test social networks
- distiguish parameters among agents
- make a theory of it...


CREDITS

This model has been designed with Tam Kien Duong and other participants during the Meze summer school http://meze.rnsc.fr
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

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
NetLogo 3.1.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
