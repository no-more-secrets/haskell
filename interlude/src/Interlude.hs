-- The purpose of this module is to just export
-- everything that it contains.
module Interlude
    ( module EIO
    , module Prelude
    , module Safety
    ) where

import EIO
    ( EIO(..)
    , eTe
    , main_
    )

import Safety
    ( E
    )

-----------------------------------------------------
-- Prelude wrappers
-----------------------------------------------------
import EIO
    ( readFile
    , putStr
    , putStrLn
    , print
    )

import Safety
    ( div
    , read
    )

-- Here we should hide anything exported by the above
-- prelude wrappers
import Prelude hiding
    ( div
    , read
    , readFile
    , putStr
    , putStrLn
    , print
    )
-----------------------------------------------------
