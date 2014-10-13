module Main where

import CV.ObjDetect
import CV.HighGUI
import CV.Image
import CV.ColourUtils

faceClassifier, eyesClassifier :: String
faceClassifier = "haarcascade_frontalface_alt.xml"
eyesClassifier = "haarcascade_eye_tree_eyeglasses.xml"

main :: IO ()
main = handle (\e -> ...) $ do
  Just face <- loadCascadeClassifier faceClassifier
  Just eyes <- loadCascadeClassifier eyesClassifier
  Just capture <- captureFromCam (-1)
  runStream_ . sideEffect (detectAndDisplay face eyes) $ streamFromVideo cap

faceOpts, eyesOpts :: CascadeClassifierParams
faceOpts = CascadeClassifierParams 1.1 2 CV_HAAR_SCALE_IMAGE (30, 30)
eyesOpts = CascadeClassifierParams 1.1 2 CV_HAAR_SCALE_IMAGE (30, 30)

faceCol, eyesCol :: Color RGB D8
faceCol = (255, 0, 255)
eyesCol = (255, 0, 0)

detectAndDisplay :: CascadeClassifier -> CascadeClassifier ->
                    Image RGB D32 -> IO ()
detectAndDisplay face eyes frame = do
  let f = equalizeHistogram $ rgbToGray8 frame
      faces = detectMultiScale face faceOpts f
      ops = flip map faces $ \r@(Rectangle x y w h) ->
        let c = (x + 0.5 * w, y + 0.5 * h)
            sz = (0.5 * w, 0.5 * h)
            as = (0.0, 360.0)
            eyes = detectMultiScale eyes eyesOpts $ getRegion (x, y) (w, h) f
            eyecirc (Rectangle ex ey ew eh) =
              let ec = (x + ex + 0.5 * ew, y + ey + 0.5 * eh)
                  rad = round $ 0.25 * (ew + eh)
              in circleOp eyeCol ec rad (Stroked 4)
        in [ellipseOp faceCol 4 c sz 0.0 as] ++ map eyecirc eyes
  showImage "Capture - Face detection" $ frame <## ops
