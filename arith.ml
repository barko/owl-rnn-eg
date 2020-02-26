(* [fold_n n f x0] folds over sequence [n-1 ... 0] using function [f]
   starting with [x0] *)
let fold_n =
  let rec loop i f x =
    if i < 0 then
      x
    else
      let x = f i x in
      loop (i-1) f x
  in
  fun n f x0 ->
    if n < 0 then
      raise (Invalid_argument "fold_n")
    else
      loop (n-1) f x0

let random_int_in_range rng ~min ~max =
  if min > max then
    raise (Invalid_argument "int_in_range")
  else
    min + (Random.State.int rng (max - min + 1))

(* generate a sequence of integers and the corresponding cumulative sum *)
let cum_sum_sequence rng ~length ~max =
  let _, sequence = fold_n length (
    fun _ (cum_sum, sequence) ->
      let x = random_int_in_range rng ~min:0 ~max in
      let cum_sum = cum_sum + x in
      cum_sum, (x, cum_sum) :: sequence
  ) (0, []) in
  List.rev sequence

(* [cum_sum_sequences rng sequence_length ~num_sequences ~max]
   generates [num_sequences] integer sequences, either of equal of
   variable lengths. The integers in the sequence have a random value
   sampled uniformly from [[0,max]]. For example,
   [cum_sum_sequences r (Equal 7) ~num_sequences:3 ~max:10] could result
   in a the following list consisting in 3 sequences, each with length 5:

   [[9, 9; 0,  9;  3, 12; 3, 15; 6, 21];
    [4, 4; 9, 13;  4, 17; 4, 21; 3, 24];
    [8, 8; 2, 10; 10, 20; 4, 24; 1, 25]]

   Note that (Equal 7) specifies that the sequence lengths should be
   identical and be randomly drawn uniformly from [[0 ... 7]]. If instead,
   [Variable v] was specified for argument [sequence_length], then each
   of the sequences generated could have a different length, each
   sequence length randomly drawn uniformly from [[0 ... v]]. Finally, if
   [`Deterministic d] is used for sequence length, then all sequences in
   the result have the identical length [d].
*)

type v =
  | Deterministic of int
  | Equal of int
  | Variable of int


let cum_sum_sequences rng (sequence_length:v) ~num_sequences ~max =
  match sequence_length with
  | Deterministic length ->
    fold_n num_sequences (
      fun _ batch ->
        let seq = cum_sum_sequence rng ~length ~max in
        seq :: batch
    ) []

  | Equal max_length ->
    let length = random_int_in_range rng ~min:1 ~max:max_length in
    fold_n num_sequences (
      fun _ batch ->
        let seq = cum_sum_sequence rng ~length ~max in
        seq :: batch
    ) []
  | Variable max_length ->
    fold_n num_sequences (
      fun _ batch ->
        let length = random_int_in_range rng ~min:1 ~max:max_length in
        let seq = cum_sum_sequence rng ~length ~max in
        seq :: batch
    ) []


(* [choose rng ~m ~n] returns a list of length [n] whose elements are
   randomly drawn from the sequence [[0 ... m-1]], uniformly and
   without replacement. For example [choose rng ~m:5 ~n:2] could
   result in list [4; 1] or in list [2; 3] *)
let choose =
  let shuffle rng a =
    for i = (Array.length a) - 1 downto 1 do
      let k = Random.State.int rng (i + 1) in
      let tmp = a.(i) in
      a.(i) <- a.(k);
      a.(k) <- tmp
    done;
  in
  fun rng ~m ~n ->
    if m <= 0 || n > m then
      raise (Invalid_argument "choose")
    else
      let a = Array.init m (fun i -> i) in
      shuffle rng a;
      let result = ref [] in
      for i = 0 to n-1 do
        result := a.(i) :: !result
      done;
      !result

(* [dot_product_sequence rng ~sequence_length ~max ~num_summed]
   creates a sequence of pairs. The first element of the pair is an
   integer in [[0 ... max-1]]. The second element of the pair is a
   boolean; exactly [num_summed] of the pairs consist of a [true]
   value, the remaining values being [false]. The sequence is
   returned, along with the sum of the integers associated with [true]
   values. For example [dot_product_sequence rng ~sequence_length:5
   ~max:10 ~num_summed:2] could return [5, [6, false; 3, true; 10,
   false; 7, false; 2, true]]. (The function is called
   [dot_product_sequence] because the resulting sum is conceptually
   the dot product of the integer sequence and a binary sequence in
   which true is encoded as [1] and [0] otherwise.) *)
let dot_product_sequence rng ~sequence_length ~max ~num_summed =
  if sequence_length < 1 || num_summed > sequence_length then
    raise (Invalid_argument "dot_product_sequence")
  else
    let integer_sequence = fold_n sequence_length (
      fun _ sequence ->
        let i = random_int_in_range rng ~min:0 ~max:max in
        i :: sequence
    ) [] in
    let binary_sequence =
      let true_index_list = choose rng ~m:sequence_length ~n:num_summed in
      let a = Array.make sequence_length false in
      List.iter (
        fun true_index ->
          a.(true_index) <- true
      ) true_index_list;
      Array.to_list a
    in
    List.fold_left2 (
      fun (dot_product, zipped) integer is_summed ->
        let dot_product =
          if is_summed then
            dot_product + integer
          else
            dot_product
        in
        let zipped = (integer, is_summed) :: zipped in
        dot_product, zipped
    ) (0, []) integer_sequence binary_sequence

(* [dot_product_sequences rng num_sequences ~sequence_length
   ~num_summed max] returns a list of sequences and ther correspoding
   dot product. Both [sequence_length] and [num_summed] are of type
   [v]. *)
let dot_product_sequences rng ~num_sequences ~sequence_length ~num_summed ~max =
  match sequence_length, num_summed with
  | Deterministic length, Deterministic ns ->
    if ns > length then
      raise (Invalid_argument "dot_product_sequences")
    else
      fold_n num_sequences (
        fun _ batch ->
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []

  | Equal max_length, Deterministic ns ->
    if ns > max_length then
      raise (Invalid_argument "dot_product_sequences")
    else
      let length = random_int_in_range rng ~min:ns ~max:max_length in
      fold_n num_sequences (
        fun _ batch ->
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []

  | Variable max_length, Deterministic ns ->
    if ns > max_length then
      raise (Invalid_argument "dot_product_sequences")
    else
      fold_n num_sequences (
        fun _ batch ->
          let length = random_int_in_range rng ~min:ns ~max:max_length in
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []

  | Deterministic length, Equal max_ns ->
    if max_ns > length then
      raise (Invalid_argument "dot_product_sequences")
    else
      let ns = random_int_in_range rng ~min:0 ~max:max_ns in
      fold_n num_sequences (
        fun _ batch ->
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []

  | Equal max_length, Equal max_ns ->
    if max_ns > max_length then
      raise (Invalid_argument "dot_product_sequences")
    else
      let ns = random_int_in_range rng ~min:0 ~max:max_ns in
      let length = random_int_in_range rng ~min:ns ~max:max_length in
      fold_n num_sequences (
        fun _ batch ->
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []

  | Variable max_length, Equal max_ns ->
    if max_ns > max_length then
      raise (Invalid_argument "dot_product_sequences")
    else
      let ns = random_int_in_range rng ~min:0 ~max:max_ns in
      fold_n num_sequences (
        fun _ batch ->
          let length = random_int_in_range rng ~min:ns ~max:max_length in
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []

  | Deterministic length, Variable max_ns ->
    if max_ns > length then
      raise (Invalid_argument "dot_product_sequences")
    else
      fold_n num_sequences (
        fun _ batch ->
          let ns = random_int_in_range rng ~min:0 ~max:max_ns in
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []

  | Equal max_length, Variable max_ns ->
    if max_ns > max_length then
      raise (Invalid_argument "dot_product_sequences")
    else
      let length = random_int_in_range rng ~min:max_ns ~max:max_length in
      fold_n num_sequences (
        fun _ batch ->
          let ns = random_int_in_range rng ~min:0 ~max:max_ns in
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []

  | Variable max_length, Variable max_ns ->
    if max_ns > max_length then
      raise (Invalid_argument "dot_product_sequences")
    else
      fold_n num_sequences (
        fun _ batch ->
          let ns = random_int_in_range rng ~min:0 ~max:max_ns in
          let length = random_int_in_range rng ~min:ns ~max:max_length in
          let seq = dot_product_sequence rng ~sequence_length:length ~max ~num_summed:ns in
          seq :: batch
      ) []



(* [bitarray_of_int ~num_bits ~i] encodes integer [i] into it a
   [num_bits]-wide binary representation. Example: [bitarray_of_int
   ~num_bits:4 ~i:3] results in float array [[|1.; 1.; 0.; 0.|]]. Note
   that the least significant bit is on the left. *)
let bitarray_of_int ~num_bits ~i =
  let max = 1 lsl num_bits in
  if i < 0 || i >= max then
    raise (Invalid_argument "bitarray_of_int")
  else
    Array.init num_bits (
      fun k ->
        let bit = 1 lsl k in
        if i land bit = 0 then
          0.0
        else
          1.0
    )

(* [make_bitarray_of_int num_bits] creates a map from an integer (ranging
   from 0 to [2^num_bits - 1]) to it corrsponding bitarray. *)
let make_bitarray_of_int num_bits =
  let num_integers = 1 lsl num_bits in
  Array.init num_integers (
    fun i ->
      bitarray_of_int ~num_bits ~i
  )

let dot_product_sequence_in_bitarrays ~in_bitarray_of_int ~out_bitarray_of_int (sum, sequence) =
  out_bitarray_of_int.(sum), List.map (
    fun (i, b) ->
      in_bitarray_of_int.(i), b
  ) sequence

(* like [dot_product_sequences],
   [make_dot_product_sequences_in_bitarrays] creates sequences and
   their (dot-product) sum. Whereas the output (sum) inputs (integers
   summed) are respresented as integers in [dot_product_sequenes], in
   [make_dot_product_sequences_in_bitarrays] they are represented as
   bit arrays. *)
let make_dot_product_sequences_in_bitarrays ~num_input_bits ~num_output_bits =
  (* the sum of [num_sum] integers, each whose bit width is
     [num_input_bits], has bit width [num_output_bits] *)
  let num_summed = (num_output_bits - num_input_bits) + 1 in
  if num_summed < 1 then
    raise (Invalid_argument "make_dot_product_sequences_in_bitarrays")
  else
    let max = (1 lsl num_input_bits) - 1 in
    let to_bitarrays =
      let in_bitarray_of_int = make_bitarray_of_int num_input_bits in
      let out_bitarray_of_int = make_bitarray_of_int num_output_bits in
      dot_product_sequence_in_bitarrays ~in_bitarray_of_int ~out_bitarray_of_int
    in
    let num_summed = Deterministic num_summed in
    fun rng num_sequences sequence_length ->
      let s = dot_product_sequences rng ~num_sequences ~sequence_length ~num_summed ~max in
      List.rev_map to_bitarrays s

