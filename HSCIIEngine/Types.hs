module HSCIIEngine.Types where

type Height = Int
type Width = Int
type Dimensions = (Width, Height)

type X = Float
type Y = Float
type Coords = (X, Y)

type Row = String
type Image = [String]

type Object = (Coords, Dimensions, Image)
