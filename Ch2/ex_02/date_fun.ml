(* Exercise: date fun [★★★]

Define a function that takes an integer d and string m as input and returns true just when d and m form a valid date. Here, a valid date has a month that is one of the following abbreviations: Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec. And the day must be a number that is between 1 and the minimum number of days in that month, inclusive. For example, if the month is Jan, then the day is between 1 and 31, inclusive, whereas if the month is Feb, then the day is between 1 and 28, inclusive.

How terse (i.e., few and short lines of code) can you make your function? You can definitely do this in fewer than 12 lines. *)

(** Valid date: 31: Jan, Mar, May, Jul, Aug, Oct, Dec 30: Apr, Jun, Sept, Nov
    28: Feb *)
let date (d : int) (m : string) =
  if
    (d >= 1 && d <= 31)
    && (m = "Jan" || m = "Mar" || m = "Jul" || m = "Aug" || m = "Oct"
      || m = "Dec")
    || (m = "Apr" || m = "Jun" || m = "Sept" || (m = "Nov" && d <= 30))
    || (m = "Feb" && d <= 28)
  then true
  else false

let _ = assert (date 1 "Jan")
let _ = assert (date 0 "Jan" |> not)
let _ = assert (date 1 "ABC" |> not)
