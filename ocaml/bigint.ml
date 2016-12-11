(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct
   
   (*Constant*)
   type sign     = Pos | Neg
   type bigint   = Bigint of sign * int list
   let  radix    = 10
   let  radixlen =  1
   let  zero     = Bigint (Pos, [])

   (*Function link*)
   let car       = List.hd
   let cdr       = List.tl
   let map       = List.map
   let reverse   = List.rev
   let strcat    = String.concat
   let strlen    = String.length
   let strsub    = String.sub

   (*Given*)
   let charlist_of_string str = 
     let last = strlen str - 1
        in  let rec charlist pos result =
         if pos < 0
         then result
         else charlist (pos - 1) (str.[pos] :: result)
      in  charlist last []

   let bigint_of_string str =
     let len = strlen str
     in  let to_intlist first =
         let substr = strsub str first (len - first) in
         let digit char = int_of_char char - int_of_char '0' in
            map digit (reverse (charlist_of_string substr))
         in  if   len = 0
             then zero
             else if   str.[0] = '_'
               then Bigint (Neg, to_intlist 1)
               else Bigint (Pos, to_intlist 0)

   let string_of_bigint (Bigint (sign, value)) =
      match value with
      | []    -> "0"
      | value -> let reversed = reverse value
         in  strcat ""
         ((if sign = Pos then "" else "-") ::
         (map string_of_int reversed))

   let rec cmp' list1 list2 = 
      match (list1, list2) with
      | [], []                 ->  0
      | list1, []              ->  1
      | [], list2              -> -1
      | car1::cdr1, car2::cdr2 -> 
         let c = cmp' cdr1 cdr2
         in if c = 0 && car1 != car2
            then (
               if car1 > car2 then 1
               else (
                  if car1 < car2 then -1
                  else 0))
         else c

   let cmp (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
      if neg1 = neg2 then cmp' value1 value2
      else if neg1 = Neg then -1
      else 1

   let rmzero list =
      let rec rmzero' list' = match list' with
         | []       -> [] (*empty to empty*)
         | [0]      -> [] (*make zero to empty*)
         | car::cdr ->
            let cdr' = rmzero' cdr
               in  match car, cdr' with
                    | 0, [] -> []
                    | car, cdr' -> car::cdr'
         in rmzero' list

   let rec add' list1 list2 carry = match (list1, list2, carry) with
      | list1, [], 0       -> list1
      | [], list2, 0       -> list2
      | list1, [], carry   -> add' list1 [carry] 0
      | [], list2, carry   -> add' [carry] list2 0
      | car1::cdr1, car2::cdr2, carry ->
         let sum = car1 + car2 + carry
         in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

   let rec sub' list1 list2 carry = match (list1, list2, carry) with
      | list1, [], 0           -> list1
      | [], list2, 0           -> 
         failwith "Error: sub' should not handle empty - list2"
      | [], list2, carry       -> 
         failwith "Error: sub' should not handle empty - list2"
      | car1::cdr1, [], carry  -> 
            if car1 = 0
            then 9 :: (sub' cdr1 [] 1)
            else let dif = car1 - carry * 1
         in dif :: (sub' cdr1 [] 0)
      | car1::cdr1, car2::cdr2, carry ->
            if car2 > (car1 - carry*1)
            then let dif = ((car1 + 10) - carry*1) - car2
         in dif :: (sub' cdr1 cdr2 1)
            else let dif = (car1 - carry*1) - car2
         in dif :: (sub' cdr1 cdr2 0)

   let make_double number = add' number number 0              

   let rec mul' (multiplier, powerof2, multiplicand') =
      if (cmp' powerof2 multiplier) = 1
      then multiplier, []
      else let remainder, product =
         mul' (multiplier, 
               make_double powerof2,
               make_double multiplicand')
      in if (cmp' powerof2 remainder) = 1
         then remainder, product
         else (rmzero(sub' remainder powerof2 0)),
              (add' product multiplicand' 0)

   let rec quorem' (dividend, powerof2, divisor') =
      if (cmp' divisor' dividend) = 1
      then [0], dividend
      else let quotient, remainder =
            quorem' (dividend, 
                     make_double powerof2,
                     make_double divisor')
      in if (cmp' divisor' remainder) = 1
         then quotient, remainder
         else  (add' quotient powerof2 0),
               (rmzero(sub' remainder divisor' 0))

   let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
      if neg1 = neg2
      then Bigint (neg1, add' value1 value2 0)
      else 
         if (cmp' value1 value2) = 1
         then Bigint (neg1, rmzero(sub' value1 value2 0))
         else Bigint (neg2, rmzero(sub' value2 value1 0))

   let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
      if neg1 = neg2
      then (if (cmp' value1 value2) = 1
         then Bigint (Pos, rmzero(sub' value1 value2 0))
         else Bigint (Neg, rmzero(sub' value2 value1 0)))
      else (if (cmp' value1 value2) = 1
         then Bigint (neg2, add' value1 value2 0)
         else Bigint (neg1, add' value2 value1 0))

   let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
      let remainder, product =
         mul' (value1, [1], value2) in
            if neg1 = neg2
            then Bigint (Pos, product)
            else Bigint (Neg, product)

   let quorem ((Bigint (neg1, value1)), (Bigint (neg2, value2))) =
      let quotient, remainder = quorem' (value1, [1], value2)
      in if neg1 = neg2
         then Bigint (Pos, quotient),Bigint (Pos, remainder)
         else Bigint (Neg, quotient),Bigint (Pos, remainder)

   let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
      let quotient, remainder = 
         quorem ((Bigint (neg1, value1)),(Bigint (neg2, value2)))
      in quotient

   let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
      let quotient, remainder = 
         quorem ((Bigint (neg1, value1)),(Bigint (neg2, value2)))
      in remainder

   let pow = add
end

