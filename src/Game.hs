module Game (main) where

import Apecs

import Game.World.TH (initWorld)
import Game.Loop (mainLoop)

main :: IO ()
main = initWorld >>= runSystem mainLoop
