{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import System.Process
import Text.Printf
import GHC.RTS.Flags (GCFlags(maxHeapSize))
import Data.List

type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float
type Note = [Pulse]
type Semitones = Float
type Beats = Float

-- A4
pitchStandard :: Hz
pitchStandard = 440.0

getHz :: Semitones -> Hz
getHz n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

bpm :: Beats
bpm = 60.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (getHz n) (beats * beatDuration)

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.5

sampleRate :: Samples
sampleRate = 48000.0

hesAPirate :: [Pulse]
hesAPirate = concat [
    getNote "fa_F4" 1,
    getNote "la_A4" 1,
    getNote "si_B4" 1,
    getNote "si_B4" 1,

    getNote "si_B4" 1,
    getNote "do_C5" 1,
    getNote "re_D5" 1,
    getNote "re_D5" 1, 

    getNote "re_D5" 1,
    getNote "mi_E5" 1,
    getNote "do_C5" 1,
    getNote "do_C5" 1,
    getNote "si_B4" 1,
    getNote "la_A4" 1,

    getNote "la_A4" 1,
    getNote "si_B4" 1,
    
    getNote "fa_F4" 1,
    getNote "la_A4" 1,
    getNote "si_B4" 1,
    getNote "si_B4" 1,

    getNote "si_B4" 1,
    getNote "do_C5" 1,
    getNote "re_D5" 1,
    getNote "re_D5" 1,

    getNote "re_D5" 1,
    getNote "mi_E5" 1,
    getNote "do_C5" 1,
    getNote "do_C5" 1,
    getNote "si_B4" 1,
    getNote "la_A4" 1,

    getNote "si_B4" 1
    ]

oneDay :: [Pulse]
oneDay = concat [ 
    getNote "la_A4" 1, 
    getNote "sib_A4sh" 1, 
    getNote "si_B4" 1, 
    getNote "si_B4" 1, 

    getNote "la_A4" 0.5, 
    getNote "re_D5" 0.5, 
    getNote "la_A4" 0.5, 
    getNote "fa_F4" 0.5, 
    getNote "la_A4" 0.5, 
    getNote "re_D5" 0.5, 
    getNote "la_A4" 0.5, 
    getNote "fa_F4" 0.5, 

    getNote "sib_A4sh" 0.5, 
    getNote "re_D5" 0.5, 
    getNote "sib_A4sh" 0.5, 
    getNote "fa_F4" 0.5, 
    getNote "sib_A4sh" 0.5, 
    getNote "re_D5" 0.5, 
    getNote "sib_A4sh" 0.5, 
    getNote "fa_F4" 0.5, 

    getNote "la_A4" 0.5,
    getNote "do_C5" 0.5,
    getNote "la_A4" 0.5,
    getNote "fa_F4" 0.5,
    getNote "la_A4" 0.5,
    getNote "do_C5" 0.5,
    getNote "la_A4" 0.5,
    getNote "fa_F4" 0.5,

    getNote "sol_G4" 0.5,
    getNote "do_C5" 0.5,
    getNote "sol_G4" 0.5,
    getNote "mi_E4" 0.5,
    getNote "sol_G4" 0.5,
    getNote "do_C5" 0.5,
    getNote "sol_G4" 0.5,
    getNote "mi_E4" 0.5,

    getNote "la_A4" 0.5, 
    getNote "re_D5" 0.5, 
    getNote "la_A4" 0.5, 
    getNote "fa_F4" 0.5,
    getNote "la_A4" 0.5, 
    getNote "re_D5" 0.5, 
    getNote "la_A4" 0.5, 
    getNote "fa_F4" 0.5,

    getNote "sib_A4sh" 0.5, 
    getNote "re_D5" 0.5, 
    getNote "sib_A4sh" 0.5, 
    getNote "fa_F4" 0.5, 
    getNote "sib_A4sh" 0.5, 
    getNote "re_D5" 0.5, 
    getNote "sib_A4sh" 0.5, 
    getNote "fa_F4" 0.5,

    getNote "la_A4" 0.5,
    getNote "do_C5" 0.5,
    getNote "la_A4" 0.5,
    getNote "fa_F4" 0.5,
    getNote "la_A4" 0.5,
    getNote "do_C5" 0.5,
    getNote "la_A4" 0.5,
    getNote "fa_F4" 0.5,

    getNote "sol_G4" 0.5,
    getNote "do_C5" 0.5,
    getNote "sol_G4" 0.5,
    getNote "mi_E4" 0.5,
    getNote "sol_G4" 0.5,
    getNote "do_C5" 0.5,
    getNote "sol_G4" 0.5,
    getNote "mi_E4" 0.5,

    getNote "la_A4" 2,
    getNote "re_D4" 1,
    getNote "mi_E4" 1,
    getNote "fa_F4" 2,

    getNote "mi_E4" 1,
    getNote "fa_F4" 1,
    getNote "sol_G4" 2,

    getNote "fa_F4" 1,
    getNote "sol_G4" 1,
    getNote "la_A4" 2,

    getNote "sol_G4" 1,
    getNote "fa_F4" 1,
    getNote "re_D4" 2,

    getNote "re_D4" 1,
    getNote "mi_E4" 1,
    getNote "fa_F4" 2,

    getNote "sol_G4" 1,
    getNote "la_A4" 1,
    getNote "sib_A4sh" 2,

    getNote "re_D4" 1,
    getNote "sol_G4" 1,
    getNote "fa_F4" 3,

    getNote "mi_E4" 2
    ]

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ zipWith3 (\ x y z -> x * y * z) release attack output
  where
    step = (hz * 2 * pi) / sampleRate

    attack :: [Pulse]
    attack = map (min 1.0) [0.0, 0.001 ..]

    release :: [Pulse]
    release = reverse $ take (length output) attack

    output = map sin $ map (* step) [0.0 .. sampleRate * duration]

getNote :: String -> Seconds -> [Pulse]
getNote n duration
    | n == "la_A4" = note 0 duration
    | n == "sib_A4sh" = note 1 duration
    | n == "si_B4" = note 2 duration
    | n == "re_D5" = note 5 duration 
    | n == "fa_F4" = note (-4) duration 
    | n == "do_C5" = note 3 duration
    | n == "sol_G4" = note (-2) duration
    | n == "mi_E4" = note (-5) duration
    | n == "re_D4" = note (-7) duration
    | n == "sol_G4sh" = note (-1) duration
    | n == "mi_E5" = note 7 duration
    | otherwise = error "empty note or duration 0" 

wave :: [Pulse]
wave = oneDay
save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()