import HSCIIEngine.Engine

import Game.Src.Intro.Splash
import Game.Src.Intro.Calibration
import Game.Src.Haskscroller.Game

main = do
  runGame [calibrate, splash, game]
