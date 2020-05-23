open Base
open Math

let solid = "*"
let empty = " "

let circle width height _radius = 
  let center = (width / 2, height / 2) in
  let rec loop x y acc =
    if y = height then
      acc
    else if x = width then
      loop 0 (y + 1) (acc ^ "\n")
    else if dist_sq (x, y) center < square _radius then
      loop (x + 1) y (acc ^ solid)
    else
      loop (x + 1) y (acc ^ empty)
  in
  loop 0 0 ""

let line width depth =
  let rec loop x y acc =
    if y = depth then
      acc
    else if x = width then
      loop 0 (y + 1) (acc ^ "\n")
    else
      loop (x + 1) y (acc ^ solid)
  in
  loop 0 0 ""

let screen width height =
  let rec loop x y acc =
    if y = height then
      acc
    else if x = width then
      loop 0 (y + 1) (acc ^ "\n")
    else 
      loop (x + 1) y (acc ^ solid)
  in
  loop 0 0 ""

let rect width height buffer =
  let rec loop x y acc =
    if y = height - buffer then
      acc
    else if x = width then
      loop 0 (y + 1) (acc ^ "\n")
    else if x >= buffer && y >= buffer && x < (width - buffer) && y < (height - buffer) then
      loop (x + 1) y (acc ^ solid)
    else
      loop (x + 1) y (acc ^ empty)
  in
  loop 0 0 ""
  
