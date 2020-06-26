type axis = X | Y | Z

let linear_transformation matrix =
  Affine.{translation = Point3.origin; linear_transformation = matrix}

let translation trans =
  Affine.{translation = trans; linear_transformation = Matrix.id}

let rotation_x angle =
  Matrix.make
    ((1.,      0.,           0.),
     (0.,  cos angle,  -.sin angle),
     (0.,  sin angle,    cos angle))
  |> linear_transformation

let rotation_y angle =
  Matrix.make
    ((  cos angle,  0.,  sin angle),
     (      0.,     1.,       0.),
     (-.sin angle,  0.,       0.))
  |> linear_transformation

let rotation_z angle =
  Matrix.make
    ((cos angle,  -.sin angle,   0.),
     (sin angle,    cos angle,   0.),
     (    0.,           0.,       1.))
  |> linear_transformation

let tranform_about point transform p =
  p
  |> Point3.add (Point3.neg point)
  |> transform
  |> Point3.add point
  
