{-#LANGUAGE TypeFamilies#-}
module Utils.GeometryClass where

class Point2D a where
   type ELP a :: *
   pt :: a -> (ELP a, ELP a)
   toPt :: (ELP a,ELP a) -> a

convertPt :: (Point2D a, Point2D b, ELP a ~ ELP b) => a -> b
convertPt = toPt . pt

class BoundingBox a where
   type ELBB a :: *
   bounds :: a -> (ELBB a, ELBB a, ELBB a, ELBB a)

class Line2D a where
   type ELL a :: *
   offsetAngle :: a -> (ELL a, Double)

class LineSegment a where
   type ELS a :: *
   startEnd :: a -> ((ELS a, ELS a),(ELS a, ELS a))

