type v =
  | Deterministic of int
  | Equal of int
  | Variable of int

val cum_sum_sequences :
  Random.State.t ->
  v ->
  num_sequences:int ->
  max:int ->
  (int * int) list list

val dot_product_sequences :
  Random.State.t ->
  num_sequences:int ->
  sequence_length:v ->
  num_summed:v ->
  max:int ->
  (int * (int * bool) list) list

val make_dot_product_sequences_in_bitarrays :
  num_input_bits:int ->
  num_output_bits:int ->
  (Random.State.t -> int -> v -> (float array * (float array * bool) list) list)

