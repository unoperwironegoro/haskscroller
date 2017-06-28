import HSCIIEngine.Engine

import Scenes.Splash
import Scenes.Calibration

main = do
  runGame [calibrate, splashloop]
