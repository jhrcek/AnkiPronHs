module Mplayer
    ( playMp3s
    , playMp3
    ) where

import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import System.Process (callProcess)


playMp3s :: NonEmpty FilePath -> IO ()
playMp3s mp3s =
    callProcess
        "mplayer"
        ( "-noautosub" -- don't try to automatically load subtitles
            :
            -- disable LIRC (Linux Infrared remote control)
            -- without this getting warning "Failed to open LIRC support. You will not be able to use your remote control."
            "-nolirc"
            : "-really-quiet"
            : toList mp3s
        )


playMp3 :: FilePath -> IO ()
playMp3 mp3 = playMp3s (mp3 :| [])
