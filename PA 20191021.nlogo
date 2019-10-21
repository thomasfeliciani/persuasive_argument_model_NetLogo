;         Last update:  21 October 2019

; extensions [nw]
globals [
  ID
  n_interactions
  number_of_edges
  Pro
  Con
  arguments_pool
  group1
  group-1
  Ngroup1
  Ngroup-1
  converged?
  end_step

  convergence_condition ; Used in BehaviorSpace experiments
  initial_polarization  ;   "

  network_degree  ; Defines the degree that is approximated in a constant degree network

  radius          ; Schelling-Zhang parameters
  satisfaction
  tolerance_threshold
  preferred_ingroup_share
  utility_all_ingroup
  segregation

  sample_for_measures   ; Export variables and outcome measures

  csv_file_name
  csv_spacer
  precisionN
  ok_groups'_measures
  mean_o_glob
  var_o_glob
  mean_o_1
  var_o_1
  mean_o_-1
  var_o_-1
  o_difference_neighborhood
  o_difference_ingroup
  o_difference_outgroup
  local_alignment
  polarization_index
  count_extremists
  count_extremists_1
  count_extremists_-1
  sample_size_for_measures
]

turtles-own [
  happy?    ; For Schelling-Zhang engine
  suitable? ; For distinguishing suitable agents from which to sample while measuring outcome variables

  group
  arguments
  opinion
  displayed_opinion
  alters_list
  alters_set
  similarities_vector
  j
  j_opinion
  j_group
]

patches-own [expected_satisfaction] ; For Schelling-Zhang engine


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;            Setup procedure            ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to setup
  clear-all
  set ID random 999999999
  show (word
    "ID: " ID
    "  n_i: " n_interactions
    "  nw: " network_structure
    "  popsize: " population_size
    "  S: " S
    "  H: " H
    "  w: " congruency_w
    "  hom_h: " homophily_h
    "  M: " Mechanism
    "  segr: " segregation
    )
  set end_step 0
  ;if data_export = true [set time_limit 10000 if population_size > 1000 [set time_limit 2000]]; Max number of interactions per agent per simulation run.
  ifelse population_size = 6400 [set sample_size_for_measures 64][set sample_size_for_measures 10]
  if population_size < sample_size_for_measures [set sample_size_for_measures population_size]
  create_world
  setup_agents
  if data_export = true [prepare_data_export]
  set converged? false
  compute_measures
  set initial_polarization polarization_index
  if export_frames? = true [export-view (word "./gif/PA_" 0 ".png")]
  reset-ticks
  if print_trajectory? = true and converged? = false [print_data]
  show "Setup complete."

end

to create_world
  if debug? = true [show (word ID " -> " "create_world")]
  random-seed ID
  ask patches [set pcolor 9]
  set Pro [] set Con [] ; Create a set of arguments
  let P 1 let C -1
  repeat pro_arguments_P [
    set Pro lput P Pro
    set P P + 1
  ]
  repeat con_arguments_C [
    set Con lput C Con
    set C C - 1
  ]
  set arguments_pool sort sentence Pro Con
  if pro_arguments_P < S [set pro_arguments_P S user-message (word "S was bigger than the set of pro arguments. Therefore, the number of pro argument was increased to S.")]
  if con_arguments_C < S [set con_arguments_C S user-message (word "S was bigger than the set of con arguments. Therefore, the number of con argument was increased to S.")]
  create_network
  set n_interactions 0
end

to create_network
  if debug? = true [show (word ID " -> " "create_network")]
  set segregation 0 ; Only used for distinguishing between segregation presets when
                    ; the concept applies (specifically, when network_structure = "Schelling-Zhang segregation".
  ifelse network_structure = "complete network" [
    crt population_size
    ;ask turtles [create-links-with other turtles ]
  ]
  [
    ifelse network_structure = "constant degree" [
      ;let b sqrt population_size
      ;nw:generate-lattice-2d turtles links b b true []
      crt population_size
      set network_degree 8 ;;;;;;;;;;;;;;;;;;;;;;;
      show (word "Creating a constant degree network with degree ≈ " network_degree ". This might take a while. To stop the procedure, click on Tools/Halt")
      ask turtles [
        let degree network_degree
        let cc 0
        while [10 * degree > cc and (count my-links) < degree]
        [
          set cc cc + 1
          let alter one-of other turtles with [count link-neighbors < degree]
          ;if nobody != alter [create-link-with alter]
          ifelse alter != nobody [create-link-with alter][create-link-with one-of other turtles]
        ]
      ]
      show "Constant degree network created."
      ;show mean([count link-neighbors] of turtles)
      ;show count turtles with [count link-neighbors = 0]
    ]
    [
      if network_structure = "Schelling-Zhang segregation" [
        run_schelling_procedure
        ask turtles [ifelse color = green [set group 1][set group -1]]
        ask patches [if not any? turtles-here [sprout 1 [set group 9]]]
        ask turtles [
          set alters_set turtles-on neighbors
          if group = 9 [ ; Assigning a group to buffer cells.
            let valid_neighbors []
            set valid_neighbors alters_set with [group = 1 or group = -1]
            let list_valid_neighbors sort valid_neighbors
            ifelse empty? list_valid_neighbors
              [ifelse random 2 = 1 [set group 1][set group -1]]
              [let random_neighbor one-of valid_neighbors set group [group] of random_neighbor]
          ]
        ]
      ]
    ]
  ]
  if network_structure != "Schelling-Zhang segregation" [
    ifelse population_size <= 200
      [layout-circle sort turtles (world-width / 2 - 1)]
      [ask turtles [setxy random-xcor random-ycor]]
  ]
  ;set number_of_edges population_size * (population_size - 1) / 2
  ;while [count links > number_of_edges][ask one-of links [die]]
end


to setup_agents
  if debug? = true [show (word ID " -> " "setup_agents")]
  create_groups
  create_opinions
  ask turtles [           ; Making interaction neighborhood consistent across models:
    if network_structure = "complete network"
      [set alters_list sort other turtles set alters_set other turtles]
    if network_structure = "constant degree"
      [set alters_list sort link-neighbors set alters_set link-neighbors]
    if network_structure = "Schelling-Zhang segregation"
      [set alters_list sort alters_set]
  ]
  create_sample_for_measures
  update_graphics
  show "Agents initialized."
end

to create_groups
  if debug? = true [show (word ID " -> " "create_agents")]
  if network_structure != "Schelling-Zhang segregation" [
    let size_group_1 symmetry_of_group_sizes * population_size / 2  ; Create groups
    ask n-of size_group_1 turtles [set group 1]
    ask turtles with [group != 1][set group -1]
  ]
  ask turtles [
    set size 2
    ifelse group = 1 [set shape"cylinder"][set shape "square"]
  ]
  set group1 turtles with [group = 1] set group-1 turtles with [group = -1]
  set Ngroup1 count group1
  set Ngroup-1 count group-1
  ifelse Ngroup1 > 1 and Ngroup-1 > 1 [set ok_groups'_measures true][set ok_groups'_measures false]
end

to create_opinions
  if debug? = true [show (word ID " -> " "create_opinions")]
  ask turtles [ ; Give agents arguments and the resulting opinion
    ;ifelse Mechanism = "Pseudo-PA"
    ;[
    ;  set opinion (1 - random-float 2)
    ;]
    ;[
    set arguments []
    let possible_P_arguments Pro
    let possible_C_arguments Con
    let o 0
    ifelse group = 1
    [
      repeat S [
      ifelse random-float 1 <= congruency_w
        [
          let p one-of possible_P_arguments
          set arguments lput p arguments
          set possible_P_arguments remove p possible_P_arguments
          ;set possible_pro
          set o o + 1
        ]
        [
          let c one-of possible_C_arguments
          set arguments lput c arguments
          set possible_C_arguments remove c possible_C_arguments
        ]
      ]
    ]
    [
      repeat S [
        ifelse random-float 1 > congruency_w
        [
          let p one-of possible_P_arguments
          set arguments lput p arguments
          set possible_P_arguments remove p possible_P_arguments
          set o o + 1
        ]
        [
          let c one-of possible_C_arguments
          set arguments lput c arguments
          set possible_C_arguments remove c possible_C_arguments
        ]
      ]
    ]
    set opinion arguments-to-opinion arguments
    ;]
    define_displayed_opinion

    if Mechanism = "Hybrid" or Mechanism = "Pseudo-PA" [set arguments []]
  ]
end

to create_sample_for_measures;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if debug? = true [show (word ID " -> " "create_sample_for_measures")]
  set sample_for_measures n-of sample_size_for_measures turtles
  repeat 10 [
    ask sample_for_measures [
      set suitable? true
      let mygroup group
      let my_set alters_set with [group = mygroup]
      let other_set alters_set with [group != mygroup]
      if count my_set = 0 or count other_set = 0
      [set suitable? false]
    ]
    ifelse all? sample_for_measures [suitable? = true]
    [stop]               ; The sample is suitable
    [                    ; The sample is unsuitable. Drawing a new one.
      ;show sort sample_for_measures
      set sample_for_measures []
      set sample_for_measures n-of sample_size_for_measures turtles
    ]
  ]
end

to update_graphics
  if debug? = true [show (word ID " -> " "update_graphics")]
  ask turtles [
    set color scale-color orange opinion -3 1 ; Agents' color represents the opinion (-1=orange ; +1=white)
  ]
  if network_structure = "Schelling-Zhang segregation" [
    ask turtles [
      if group = -1 [ set label "*" set label-color black]
    ]
  ]
end

to-report arguments-to-opinion [arguments_vector]
  let number_of_pro_arg length filter [ [?] -> ? > 0 ] arguments_vector
  report precision ((number_of_pro_arg / S * 2) - 1) 5
end

to define_displayed_opinion
  if Agents_displayed_opinion = "Continuous" [set displayed_opinion opinion]
  if Agents_displayed_opinion = "Dichotomous" [
    let o (opinion + 1)/ 2
    let r random-float 1
    ifelse o >= r [set displayed_opinion 1][set displayed_opinion -1]
  ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;              Go procedure             ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to go
  set converged? true
  make_interactions
  tick
  if (ticks mod print_frequency) = 0 [
    compute_measures
    if print_trajectory? = true and converged? = false and ticks < time_limit [print_data]
  ]
  if export_frames? = true [export-view (word "./gif/PA_" ticks ".png")]
  if converged? = true or (ticks >= time_limit) [
    set end_step 1
    if data_export = true [compute_measures print_data]
    stop
  ]
  ;show sort([opinion] of turtles)
end

to make_interactions
  if debug? = true [show (word ID " -> " "make_interactions")]
  if Mechanism = "Original PA"[
    ask turtles [
      compute_similarities
      pick_interaction_partner
      let a []
      ask j [set a one-of arguments]
      ifelse member? a arguments
      [          ; ineffectvie argument exchange
        set arguments remove a arguments
        set arguments lput a arguments
      ]
      [          ; argument exchange
        set arguments lput a arguments
        set arguments remove (first arguments) arguments
        set opinion arguments-to-opinion arguments
        define_displayed_opinion
      ]
      set n_interactions n_interactions + 1
    ]
  ]
  if Mechanism = "Pseudo-PA" [
    ask turtles [
      compute_similarities
      set j one-of alters_set
      set j_opinion [opinion] of j
      set j_group [group] of j
      let w (1 + (1 - ((abs (j_opinion - opinion) * H + abs (j_group - group )) / ( 1 + H ) ))) / 2 ; note to self: this could be more efficient as it's already calculated in compute_similarities
      ;print(word "w=" precision (w) 3 "  ^H_p=" precision (w ^ pseudo_homophily) 3 "   pseudo-a=" pseudo-a "    opinion=" opinion "    opinion2=" precision (opinion + pseudo-a * (w ^ pseudo_homophily)) 5)
      set opinion precision (opinion + pseudo-a * (w ^ pseudo_homophily)) 5
      ifelse opinion > 1 [set opinion 1][if opinion < -1 [set opinion -1]]
      define_displayed_opinion
      set n_interactions n_interactions + 1
    ]
  ]
  if Mechanism = "Hybrid"[
    ask turtles [
      compute_similarities
      pick_interaction_partner
      set j_opinion [opinion] of j
      set j_group [group] of j
      set opinion precision (opinion + pseudo-a) 5
      ifelse opinion > 1 [set opinion 1][if opinion < -1 [set opinion -1]]
      define_displayed_opinion
      set n_interactions n_interactions + 1
    ]
  ]
  update_graphics
end

to compute_similarities
  set similarities_vector []
  foreach alters_list [ [?1] ->
    ;let similarity (2 - abs(([group] of ?) - group) + 2 - abs(([displayed_opinion] of ?) - opinion)) / 4
    let similarity (2 - abs(([group] of ?1) - group) + H * (2 - abs(([opinion] of ?1) - opinion))) / (2 + 2 * H)
    set similarities_vector lput similarity similarities_vector

; Convergence test
    if converged? = true [
      if Mechanism = "Original PA" [
        let same_arguments? true
        foreach [arguments] of ?1 [ [??1] -> if member? ??1 arguments = false [set same_arguments? false] ] ; If all j's arguments are in i's arguments set, then same_arguments? is true
        if similarity != 0 and same_arguments? = false
        [set converged? false]
      ]
      if Mechanism = "Hybrid" or Mechanism = "Pseudo-PA" [
        if similarity != 0 and ([precision (displayed_opinion) 5] of ?1) - (precision (opinion) 5) != 0 ; If i and j can further influence each other (similarity > 0 or they don't already agree on an extreme opinion)
        [set converged? false]
      ]
    ]
  ]
end

to pick_interaction_partner
  let interaction_probabilities []
  let denominator sum map [ [?1] -> ?1 ^ homophily_h ] similarities_vector ; sum(similarities_vector) ^ homophily_h
  ifelse denominator = 0 [    ;if i has max dissimilarity with all link-neighbors, then j is picked with uniform distribution ...
    set j one-of alters_list
  ]
  [                           ;else, j is picked with a probability which is function of the similarity.
    let index 0
    let _count length alters_list
    let sum_probabilities_vector 0
    while [ index < _count][
      let prob ((item index similarities_vector)  ^ homophily_h) / denominator
      set interaction_probabilities lput prob interaction_probabilities
      set sum_probabilities_vector sum_probabilities_vector + prob
      set index index + 1
    ]
    let r random-float sum_probabilities_vector
    set index 0
    while [index < _count][
      set r (r - item index interaction_probabilities)
      if r < 0
      [
        set j item index alters_list
        stop
      ]
      set index index + 1
    ]
  ]
end

to-report pseudo-a
  ifelse (( j_opinion + 1 ) / 2 ) > random-float 1 [  ; If j picks a pro argument...
    ifelse (( opinion + 1 ) / 2 ) > random-float 1 [  ; ...and i drops a pro argument, then a=0 (ineffective argument exchange)
      report 0
    ]
    [                                                 ; ...and i drops a con argument, then i's opinion gets a positive push
      report (2 / S)
    ]
  ]
  [                                                   ; If j picks a con argument
    ifelse (( opinion + 1 ) / 2 ) > random-float 1 [  ; ...and i drops a pro argument, then i's opinion gets a negative push
      report (-2 / S)
    ]
    [                                                 ; ...and i drops a con argument, then a=0 (ineffective argument exchange)
      report 0
    ]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;          Measures and output          ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to compute_measures
  if debug? = true [show (word ID " -> " "compute_measures")]
  set precisionN 5
  set mean_o_glob precision (mean [opinion] of turtles) precisionN
  set var_o_glob precision (variance [opinion] of turtles) precisionN
  if ok_groups'_measures = true [
    set mean_o_1 precision (mean [opinion] of group1) precisionN
    set var_o_1 precision (variance [opinion] of group1) precisionN
    set mean_o_-1 precision (mean [opinion] of group-1) precisionN
    set var_o_-1 precision (variance [opinion] of group-1) precisionN
  ]

;;;;;;; Computing alignment ---------------------------------------------------
    let diff_neigh []
    let diff_ingr []
    let diff_outgr []
  ask sample_for_measures [
    let mean_o_neigh mean [opinion] of alters_set
    set diff_neigh lput (abs(opinion - mean_o_neigh)) diff_neigh
    let mygroup group
    let my_set alters_set with [group = mygroup]
    if count my_set > 0 [
      let mean_o_ingroup mean [opinion] of my_set
      set diff_ingr lput (abs(opinion - mean_o_ingroup)) diff_ingr
    ]
    set my_set alters_set with [group != mygroup]
    if count my_set > 0 [
      let mean_o_outgroup mean [opinion] of my_set
      set diff_outgr lput (abs(opinion - mean_o_outgroup)) diff_outgr
    ]
  ]
  ifelse empty? diff_neigh [set o_difference_neighborhood 9][set o_difference_neighborhood precision ( mean diff_neigh ) precisionN]
  ifelse empty? diff_ingr [set o_difference_ingroup 9][set o_difference_ingroup precision ( mean diff_ingr ) precisionN]
  ifelse empty? diff_outgr [set o_difference_ingroup 9][set o_difference_outgroup precision ( mean diff_outgr ) precisionN]
  set local_alignment precision (o_difference_outgroup - o_difference_ingroup) precisionN

  ;;;;;;; Computing polarization measures --------------------------------------
  ifelse ok_groups'_measures = false
  [set polarization_index 9]
  [
    let opinion_distances []
    ask sample_for_measures [
    let my_opinion opinion
      ask other sample_for_measures [
        set opinion_distances lput (abs (my_opinion - opinion)) opinion_distances
      ]
    ]
    set polarization_index precision (variance opinion_distances) precisionN
  ]
  let extremists turtles with  [ opinion >= 0.999999999 or opinion <= -0.999999999 ]
  set count_extremists_1 count extremists with [group = 1]
  set count_extremists_-1 count extremists with [group = -1]
  set count_extremists count_extremists_1 + count_extremists_-1
end



to prepare_data_export
  set-current-directory Export_directory
  set csv_spacer ";"
  set csv_file_name (word "RawDataset_run_" behaviorspace-run-number ".csv")
end

to print_data
    let c csv_spacer
    file-open csv_file_name
    file-print (word
      ID c
      ticks c
      n_interactions c
      network_structure c
      population_size c
      Ngroup1 c
      Ngroup-1 c
      number_of_edges c
      pro_arguments_P c
      con_arguments_C c
      S c
      H c
      congruency_w c
      initial_polarization c
      homophily_h c
      Mechanism c
      mean_o_glob c
      var_o_glob c
      mean_o_1 c
      var_o_1 c
      mean_o_-1 c
      var_o_-1 c
      o_difference_neighborhood c
      o_difference_ingroup c
      o_difference_outgroup c
      polarization_index c
      count_extremists c
      count_extremists_1 c
      count_extremists_-1 c
      segregation c
      pseudo_homophily c
      time_limit c
      end_step
    )
    file-close
end







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;          Misc          ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to run_schelling_procedure
  ;;;;; Schelling-Zhang parameters. Aligned with the presets adopted by Feliciani, Flache, Tolsma (2017).
  if Schelling_presets = "1- Low segregation" [
    set segregation 1
    set preferred_ingroup_share 0.7
    set radius 1
  ]
  if Schelling_presets = "2- Medium segregation" [
    set segregation 2
    set preferred_ingroup_share 0.8
    set radius 3
  ]
   if Schelling_presets = "3- High segregation" [
    set segregation 3
    set preferred_ingroup_share 0.9
    set radius 5
  ]
  let iterations 50
  let %_buffer_cells 15
  set tolerance_threshold 0.5
  set utility_all_ingroup 0.5
  let world_width round(sqrt(population_size))
  set population_size world_width ^ 2

  let Neighborhood_size 8
  ;set satisfaction 0.7
  ;let tile_size 1


  ;;;;; Segregation procedure
  ;set-patch-size tile_size
  resize-world 0 (world_width - 1 ) 0 (world_width - 1 )
  let N count patches
  if Neighborhood_size >= (N - 2) [set Neighborhood_size (N - 2) show "Population_size is too small. Neighborhood_size has been reduced accordingly."]
  show "Starting Schelling engine. This might take a while. To stop the procedure, click on Tools/Halt"
  let size_buffer %_buffer_cells * N / 100
  ask n-of (N - size_buffer) patches [sprout 1 [set color red]]
  ask n-of (symmetry_of_group_sizes * (count turtles) / 2) turtles [ set color green ]
;  ask one-of patches [ set cells_in_radius count patches in-radius Radius ]

  let count_iterations 0

  let attempts 0
  repeat iterations [
    let allHappy true
    let noneMoved true
    ask turtles [
      let mycolor color
      let turtles-nearby turtles in-radius radius
      if count turtles-nearby > 0[
        let similar-nearby count turtles-nearby  with [color = mycolor]
        let other-nearby count turtles-nearby with [color != mycolor]
        set satisfaction U similar-nearby other-nearby
        ifelse satisfaction >= tolerance_threshold [ set happy? true ] [ set happy? false ]
        if not happy? [
          set allHappy false
          let number_possible_spots size_buffer ;* 0.5    ;;;
          let possible_spots n-of number_possible_spots patches with [ count turtles-here = 0 ]
          ask possible_spots [
            set turtles-nearby turtles in-radius Radius
            ifelse count turtles-nearby > 0[
              let count_ingroup count turtles-nearby with [ color = mycolor ]
              let count_outgroup count turtles-nearby with [ color != mycolor ]
              set expected_satisfaction U count_ingroup count_outgroup
            ][
              set expected_satisfaction 0
            ]
          ]
          let satisfactory_spots possible_spots with [ expected_satisfaction > tolerance_threshold ] ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ifelse count satisfactory_spots > 0 [
            move-to one-of satisfactory_spots
            set noneMoved false
          ]
          [
            let var satisfaction
            let better_spots possible_spots with [ expected_satisfaction > var ]
            if count better_spots > 0 [
              move-to one-of better_spots
              set noneMoved false
            ]
          ]
        ]
      ]
    ]
    if allHappy [
      show ( word "Schelling engine terminated at iteration " count_iterations " (all agents satisfied).")
      stop
    ]
    if noneMoved [
      set attempts attempts + 1
      if attempts  > 3 [
        show ( word "Schelling engine terminated at iteration " count_iterations " (NOT all agents satisfied).")
        stop
      ]
    ]
    set count_iterations count_iterations + 1
  ]
show "Schelling engine terminated iterations. Not all agents are satisfied."
end

to-report U [ similar-nearby other-nearby ]
  let total-nearby similar-nearby + other-nearby
  let fp preferred_ingroup_share * total-nearby
  ifelse similar-nearby <= fp
  [ report similar-nearby / fp ]
  [ report utility_all_ingroup + (( other-nearby * ( 1 - utility_all_ingroup )) / ( total-nearby * ( 1 - Preferred_ingroup_share ) )) ]
end
@#$#@#$#@
GRAPHICS-WINDOW
316
54
768
507
-1
-1
12.0
1
10
1
1
1
0
0
0
1
0
36
0
36
0
0
1
ticks
30.0

BUTTON
101
15
165
48
Setup
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
165
15
229
48
Go Once
go
NIL
1
T
OBSERVER
NIL
O
NIL
NIL
1

BUTTON
229
15
294
48
Go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
1

CHOOSER
17
466
155
511
Mechanism
Mechanism
"Original PA" "Hybrid" "Pseudo-PA"
2

SLIDER
13
206
185
239
pro_arguments_P
pro_arguments_P
1
15
10.0
1
1
NIL
HORIZONTAL

SLIDER
13
270
185
303
S
S
2
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
13
137
185
170
symmetry_of_group_sizes
symmetry_of_group_sizes
0
1
1.0
0.05
1
NIL
HORIZONTAL

TEXTBOX
15
171
200
193
0 = All agents belong to the same group\n1 = Population equally split in two groups
9
0.0
1

SLIDER
13
347
185
380
congruency_w
congruency_w
0
1
0.8
0.05
1
NIL
HORIZONTAL

SLIDER
13
238
185
271
con_arguments_C
con_arguments_C
1
15
10.0
1
1
NIL
HORIZONTAL

MONITOR
185
217
308
262
Size of arguments' pool
length arguments_pool
0
1
11

SLIDER
13
380
185
413
homophily_h
homophily_h
0
10
5.0
0.1
1
NIL
HORIZONTAL

SWITCH
314
581
439
614
data_export
data_export
1
1
-1000

INPUTBOX
439
572
536
632
print_frequency
10.0
1
0
Number

INPUTBOX
328
632
620
692
export_directory
NIL
1
0
String

PLOT
834
10
1175
130
Average opinion
tick
opinion
0.0
10.0
-1.0
1.0
true
true
"" ""
PENS
"⬛ Group 1" 1.0 0 -7500403 true "" "plot mean_o_1"
"O Group -1" 1.0 0 -16777216 true "" "plot mean_o_-1"
"Whole population" 1.0 0 -8630108 true "" "plot mean_o_glob"

PLOT
834
130
1186
250
Opinion polarization
Time
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Prop. of extremists" 1.0 0 -16777216 true "" "plot count_extremists / population_size"
"Polarization index" 1.0 0 -2674135 true "" "plot polarization_index"

MONITOR
14
10
102
55
Simulation seed
ID
0
1
11

MONITOR
317
10
434
55
Number of interactions
n_interactions
0
1
11

CHOOSER
17
511
186
556
Agents_displayed_opinion
Agents_displayed_opinion
"Dichotomous" "Continuous"
1

TEXTBOX
330
692
530
731
In order to export data, make sure that the specified directory already exists.
10
0.0
1

INPUTBOX
13
77
99
137
population_size
10.0
1
0
Number

MONITOR
434
10
501
55
NIL
converged?
0
1
11

SLIDER
13
302
185
335
H
H
0
5
3.0
0.05
1
NIL
HORIZONTAL

TEXTBOX
187
303
337
336
Relative weight of opinion and group identification (for H=>infinity only opinion counts)
9
0.0
1

SWITCH
642
588
768
621
export_frames?
export_frames?
1
1
-1000

TEXTBOX
773
590
923
618
For this feature to work create a folder ./gif/
11
0.0
1

PLOT
833
369
1033
489
Opinion distribution
opinion
freq.
-1.0
1.06
0.0
10.0
true
true
"" ""
PENS
"default" 0.05 1 -16777216 false "" "histogram [opinion] of turtles"

CHOOSER
99
77
314
122
network_structure
network_structure
"complete network" "constant degree" "Schelling-Zhang segregation"
1

PLOT
834
250
1251
370
Alignment of opinion and group identity
Time
NIL
0.0
10.0
-2.0
2.0
true
true
"" ""
PENS
"(origin)" 1.0 0 -1513240 true "" "plot 0"
"Opinion difference with outgroup" 1.0 0 -2139308 true "" "plot o_difference_outgroup"
"Opinion difference with ingroup" 1.0 0 -8330359 true "" "plot o_difference_ingroup"
"Alignment" 1.0 0 -16777216 true "" "plot local_alignment"

CHOOSER
17
556
186
601
Schelling_presets
Schelling_presets
"1- Low segregation" "2- Medium segregation" "3- High segregation"
1

SWITCH
645
690
748
723
debug?
debug?
1
1
-1000

SLIDER
13
412
185
445
pseudo_homophily
pseudo_homophily
0
10
5.0
1
1
NIL
HORIZONTAL

INPUTBOX
87
644
242
704
time_limit
500000.0
1
0
Number

SWITCH
642
624
791
657
print_trajectory?
print_trajectory?
1
1
-1000

@#$#@#$#@
# Persuasive argument model

## WHAT IS IT?

This model allows us to compare alternative implementation of the persuasive argument model (PA model, for short). The PA model is based on the Persuasive Argument Theory (Vinokur & Burnstein, 1978). It was designed to help us explore the potential consequences of a social influence process called persuasive argument communication, whereby individuals form or adjust their opinion by acquiring new arguments from their social environment.

## HOW IT WORKS

Agents (the nodes in the network) have two main attributes: their demographic group (i.e. circle or square), and their opinion about something (a real value on a scale from white to orange). We can think of opinion as agents’ political orientation, from white (say, extreme left) to orange (extreme right). Agents’ opinion is based on a list of arguments, which can be either pro-white or pro-orange. So, for example, an agent who only knows pro-orange arguments displays a 100% orange opinion (in our example, this corresponds to an extreme right-wing supporter). Accordingly, this agent will be visualized as an orange node in the NetLogo interface.
During each step of a simulation run, two things happen. First, agents select an interaction partner. Following the rule of homophily, agents interact more with alters who are similar to them. For example, interactions are more likely to occur between agents from the same demographic group (circle vs square) or with a similar opinion.
Second, agents learn an argument from their interaction partner. This imitates a ‘conversation’ between the two agents, where an agent chooses an argument she knows and communicates it to her interaction partner. At the same time, the interaction partner ‘learns’ the new argument and forgets an old one. This is done by replacing one of her previously known arguments with the new one she received during the interaction.

The simulation terminates when agents cannot influence one another any further: this can happen, for example, when all agents have the same set of arguments, so no new information can be circulated, and thus no agent will ever be able to further adjust their opinion.


## HOW TO USE IT

The main parameters found in the interface are:

### Mechanism
This setting allows to select which version of the PA model to run the model with. Model versions differ from one another in two dimensions: (1) whether arguments are explicitly or implicitly modeled; (2) whether homophily is implemented as likelihood of interaction between agents (similar agents interact more often), or as effectiveness of their interaction (similar agents send each other bigger opinion pushes).
"Original-PA": explicit arguments, homophily = likelihood;
"Hybrid": implicit arguments, homophily = likelihood;
"Pseudo-PA": implicit arguments, homophily = effectiveness
### Population size
The number of nodes in the network.
### Network structure
The structural interaction network, which determines who has the possibility to interact with whom. There are three presets: "complete network" (self-explanatory), "constant degree", and "Schelling-Zhang segregation". The "constant degree" setting creates a sparse network approximating a constant degree network with degree =6. The "Schelling-Zhang segregation" network uses a Schelling-like segregation model to arrange agents on the grid. In this latter case, the interaction network is defined as the Moore neighborhood on the grid (i.e. each agent interacts with the agents placed on the surrounding 8 cells)
### Simmetry of group sizes
Defines how many agents are square and how many are circle. If the parameter is set to 0, then all agents are square. If 1, then the population is equally split in circles and squares.
### Pro arguments and Con arguments
How many arguments (pro and con a given opinion) exist in the environment?  
### S (memory size)
How many arguments can each agent hold in memory?
### H
Some model processes depend on the similarity between the interacting agents - like the selection of interaction partners or the magnitude of the opinion push that an interaction partner can give. The parameter H defines what similarity means: with H=0, similarity is only based on group similarity between agents; for H=+inf, similarity means similarity in opinion.
### Congruency (or "initial opinion bias")
This parameter determines how arguments (and thus opinions) are initially distributed in the population. High congruency means that agents from group 1 (the circle) tend to start out with many pro arguments (thus a positive opinion, displayed in color white). Conversely, group -1 (the squares) tend to start out with more con arguments (negative opinion, which is displayed in orange).
With low congruency (e.g. congruency=0.5), arguments are given at random and do not correlate with agents' group.
### Homophily strength
Defines how strong is agent's tendency to interact more often with their network neighors who are more similar to them. Low homophily causes interactions to occur at random between agents who share a link. Strong homophily means that interactions occur more often on network links between nodes who are similar in opinion (color) and group (shape). Here, homophily is captured by two parameters: homophily_h and pseudo_homophily. This is because homophily is implemented in different ways in different model versions: homophily_h is used in the Original-PA and Hybrid versions, whereas pseudo_homophily is used in the Pseudo-PA model version.
### Displayed opinion
DO agents show their true opinion to their interaction partners ("continuous")? Or do they only signal whether their opinion is positive ("dichotomous")?

## THINGS TO TRY
This model was developed to test alternative implementations of the PA model under the same conditions. It can also be used to understand the conditions making one or the other opinion outcomes more or less likely.
Suppose, for example, that we want to prevent deadlocked deliberative groups.
In other words, we want to prevent bi-polarization from emerging.
A good starting research question could be: under which conditions is bi-polarization less likely to emerge?


## REFERENCES

Mäs, M., & Flache, A. (2013). Differentiation without distancing. Explaining bi-polarization of opinions without negative influence. PloS one, 8(11), e74516.

Vinokur, A., & Burnstein, E. (1978). Depolarization of attitudes in groups. Journal of Personality and Social Psychology, 36(8), 872-885.
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="trajectories" repetitions="200" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="print_trajectory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
      <value value="&quot;Hybrid&quot;"/>
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_limit">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="homophily_h">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pseudo_homophily">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A - study 1 - Pseudo-PA" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;constant degree&quot;"/>
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
      <value value="100"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.25"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A - study 2 - Hybrid" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Hybrid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;Schelling-Zhang segregation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Schelling_presets">
      <value value="&quot;1- Low segregation&quot;"/>
      <value value="&quot;2- Medium segregation&quot;"/>
      <value value="&quot;3- High segregation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="6400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.25"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <steppedValueSet variable="homophily_h" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A - study 2 - Pseudo-PA" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;Schelling-Zhang segregation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Schelling_presets">
      <value value="&quot;1- Low segregation&quot;"/>
      <value value="&quot;2- Medium segregation&quot;"/>
      <value value="&quot;3- High segregation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="6400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.25"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="test" repetitions="2" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congruency_w">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_h">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1a" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1a/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
      <value value="&quot;Hybrid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;constant degree&quot;"/>
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
      <value value="100"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.25"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <steppedValueSet variable="homophily_h" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1b_h03" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1c_h03/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A2a" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Hybrid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;Schelling-Zhang segregation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Schelling_presets">
      <value value="&quot;1- Low segregation&quot;"/>
      <value value="&quot;2- Medium segregation&quot;"/>
      <value value="&quot;3- High segregation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="6400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.25"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <steppedValueSet variable="homophily_h" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A2b" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;Schelling-Zhang segregation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Schelling_presets">
      <value value="&quot;1- Low segregation&quot;"/>
      <value value="&quot;2- Medium segregation&quot;"/>
      <value value="&quot;3- High segregation&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="6400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.25"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1bdebug" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="debug?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;constant degree&quot;"/>
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
      <value value="100"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.25"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1a_int" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1a_int/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
      <value value="&quot;Hybrid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <steppedValueSet variable="homophily_h" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1b_int" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1b_int&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1a_int2" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1a_int2/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congruency_w">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_h">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1a_int3" repetitions="2" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1a_int3/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
      <value value="&quot;Hybrid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <steppedValueSet variable="homophily_h" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1a_int4" repetitions="2" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1a_int4/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
      <value value="&quot;Hybrid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <steppedValueSet variable="homophily_h" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1a_PC" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1a_PC/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
      <value value="&quot;Hybrid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congruency_w">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_h">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1a_h03" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1c_h03/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Original PA&quot;"/>
      <value value="&quot;Hybrid&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <steppedValueSet variable="homophily_h" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1b" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1b/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;constant degree&quot;"/>
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
      <value value="100"/>
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="congruency_w" first="0.5" step="0.1" last="0.9"/>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1b_homophily_manipulation" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="export_directory">
      <value value="&quot;/home/p271879/Desktop/PAalignment/A1b_homophily_manipulation/&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congruency_w">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pseudo_homophily">
      <value value="0"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="trajectories2" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="print_trajectory?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;complete network&quot;"/>
      <value value="&quot;constant degree&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_limit">
      <value value="500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congruency_w">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="homophily_h">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pseudo_homophily">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="A1c_pseudoPA with all networks" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <enumeratedValueSet variable="Mechanism">
      <value value="&quot;Pseudo-PA&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network_structure">
      <value value="&quot;constant degree&quot;"/>
      <value value="&quot;complete network&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time_limit">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population_size">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="H">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congruency_w">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pseudo_homophily">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="data_export">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="export_frames?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Agents_displayed_opinion">
      <value value="&quot;Continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="symmetry_of_group_sizes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="print_frequency">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pro_arguments_P">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="con_arguments_C">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
