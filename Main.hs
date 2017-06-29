import HSCIIEngine.Engine

import Scenes.Splash
import Scenes.Calibration
import Scenes.Haskscroller.Game

main = do
  runGame [calibrate, splash, game]
