module Format.Numeric (
    showDouble
) where

import Numeric

showDouble :: Double -> String
showDouble x = showFFloat Nothing x ""