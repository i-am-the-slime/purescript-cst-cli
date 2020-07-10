module Main where

import           Lib                            ( parseFile
                                                , createPSTypes
                                                )
import           Protolude

main :: IO ()
main = do
  createPSTypes
  getContents >>= parseFile
