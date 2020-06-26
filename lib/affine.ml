type t = {
  translation : Point3.t;
  linear_transformation : Matrix.t
}

let apply {translation; linear_transformation} p =
  p
  |> Matrix.apply linear_transformation
  |> Point3.add translation


let id = {translation = Point3.origin; linear_transformation = Matrix.id}
