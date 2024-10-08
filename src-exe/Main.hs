{-# LANGUAGE ApplicativeDo #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson hiding (Options)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as B
import Data.Fixed
import Data.List (intercalate)
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import Options.Applicative
import System.IO
import Text.Printf
import Text.URI qualified as URI
import Prelude hiding (reverse)

class AnytoneRender c where
  render :: c -> String

instance AnytoneRender Int where
  render t = anytoneQuote $ show t

instance AnytoneRender String where
  render = anytoneQuote

newtype Freq = Freq Micro
  deriving newtype (Read, Show, Eq, Ord, Num, Enum, Fractional)

anytoneQuote :: String -> String
anytoneQuote s = '"' : s ++ ['"']

instance AnytoneRender Freq where
  render m = anytoneQuote $ show m

data ChannelType = Analog | Digital

instance AnytoneRender ChannelType where
  render Analog = anytoneQuote "A-Analog"
  render Digital = anytoneQuote "D-Digital"

data TxPower = Low | Medium | High | Turbo

instance AnytoneRender TxPower where
  render Low = anytoneQuote "Low"
  render Medium = anytoneQuote "Medium"
  render High = anytoneQuote "High"
  render Turbo = anytoneQuote "Turbo"

data BandWidth = BW_12_5 | BW_25

instance AnytoneRender BandWidth where
  render BW_12_5 = anytoneQuote "12.5K"
  render BW_25 = anytoneQuote "25K"

data CTCSS_DCS = ToneOff

instance AnytoneRender CTCSS_DCS where
  render ToneOff = anytoneQuote "Off"

data Contact = Local

instance AnytoneRender Contact where
  render Local = anytoneQuote "Local"

data ContactCallType = GroupCall | PrivateCall

instance AnytoneRender ContactCallType where
  render GroupCall = anytoneQuote "GroupCall"
  render PrivateCall = anytoneQuote "PrivateCall"

data BusyLock = BusyLockOff

instance AnytoneRender BusyLock where
  render BusyLockOff = "Off"

data SquelchMode = Carrier

instance AnytoneRender SquelchMode where
  render Carrier = anytoneQuote "Carrier"

data OptionalSignal = OptionalSignalOff

instance AnytoneRender OptionalSignal where
  render OptionalSignalOff = anytoneQuote "Off"

data DtmfId = DtmfIdOff

instance AnytoneRender DtmfId where
  render DtmfIdOff = anytoneQuote "Off"

data TwoToneId = TwoToneId Int

instance AnytoneRender TwoToneId where
  render (TwoToneId i) = anytoneQuote $ show i

data FiveToneId = FiveToneId Int

instance AnytoneRender FiveToneId where
  render (FiveToneId i) = anytoneQuote $ show i

data PttId = PttIdOff

instance AnytoneRender PttId where
  render PttIdOff = anytoneQuote "Off"

data ColorCode = ColorCode Int

instance AnytoneRender ColorCode where
  render (ColorCode i) = anytoneQuote $ show i

data Slot = Slot Int

instance AnytoneRender Slot where
  render (Slot i) = anytoneQuote $ show i

data Scanlist = ScanlistNone

instance AnytoneRender Scanlist where
  render ScanlistNone = anytoneQuote "None"

data ReceiveGroupList = ReceiveGroupListNone

instance AnytoneRender ReceiveGroupList where
  render ReceiveGroupListNone = anytoneQuote "None"

data PttProhibit = PttProhibitOff | PttProhibitOn

instance AnytoneRender PttProhibit where
  render PttProhibitOff = anytoneQuote "Off"
  render PttProhibitOn = anytoneQuote "On"

data Reverse = ReverseOff

instance AnytoneRender Reverse where
  render ReverseOff = anytoneQuote "Off"

data SimplexTDMA = SimplexTDMAOff

instance AnytoneRender SimplexTDMA where
  render SimplexTDMAOff = anytoneQuote "Off"

data SlotSuit = SlotSuitOff

instance AnytoneRender SlotSuit where
  render SlotSuitOff = anytoneQuote "Off"

data AESDigiEncryption = NormalEncryption

instance AnytoneRender AESDigiEncryption where
  render NormalEncryption = anytoneQuote "Normal Encryption"

data DigitalEncryption = DigitalEncryptionOff

instance AnytoneRender DigitalEncryption where
  render DigitalEncryptionOff = anytoneQuote "Off"

data CallConfirmation = CallConfirmationOff

instance AnytoneRender CallConfirmation where
  render CallConfirmationOff = anytoneQuote "Off"

data TalkAroundSimplex = TalkAroundSimplexOff

instance AnytoneRender TalkAroundSimplex where
  render TalkAroundSimplexOff = anytoneQuote "Off"

data WorkAlone = WorkAloneOff

instance AnytoneRender WorkAlone where
  render WorkAloneOff = anytoneQuote "Off"

newtype Tone = Tone Deci
  deriving newtype (Show, Read, Eq, Ord, Num)

data CustomCTCSS = CustomCTCSS Tone

instance AnytoneRender CustomCTCSS where
  render (CustomCTCSS t) = anytoneQuote $ show t

data TwoToneDecode = TwoToneDecode Int

instance AnytoneRender TwoToneDecode where
  render (TwoToneDecode i) = anytoneQuote $ show i

data Ranging = RangingOff

instance AnytoneRender Ranging where
  render RangingOff = anytoneQuote "Off"

data ThroughMode = ThroughModeOn | ThroughModeOff

instance AnytoneRender ThroughMode where
  render ThroughModeOn = anytoneQuote "On"
  render ThroughModeOff = anytoneQuote "Off"

data APRSRx = APRSRxOff

instance AnytoneRender APRSRx where
  render APRSRxOff = anytoneQuote "Off"

data AnalogAPRSPttMode = AnalogAPRSPttModeOff

instance AnytoneRender AnalogAPRSPttMode where
  render AnalogAPRSPttModeOff = anytoneQuote "Off"

data DigitalAPRSPttMode = DigitalAPRSPttModeOff

instance AnytoneRender DigitalAPRSPttMode where
  render DigitalAPRSPttModeOff = anytoneQuote "Off"

data AprsReportType = AprsReportTypeOff

instance AnytoneRender AprsReportType where
  render AprsReportTypeOff = anytoneQuote "Off"

data DigitalAPRSReportChannel = DigitalAPRSReportChannel Int

instance AnytoneRender DigitalAPRSReportChannel where
  render (DigitalAPRSReportChannel i) = anytoneQuote $ show i

type Hertz = Int

data CorrectFreq = CorrectFreq Hertz

instance AnytoneRender CorrectFreq where
  render (CorrectFreq i) = anytoneQuote $ show i

data SmsConfirmation = SmsConfirmationOff

instance AnytoneRender SmsConfirmation where
  render SmsConfirmationOff = anytoneQuote "Off"

data ExcludeChannelFromRoaming = ExcludeChannelFromRoaming Int

instance AnytoneRender ExcludeChannelFromRoaming where
  render (ExcludeChannelFromRoaming i) = anytoneQuote $ show i

data DMRMode = DMRMode Int

instance AnytoneRender DMRMode where
  render (DMRMode i) = anytoneQuote $ show i

data DataACKDisable = DataACKDisable Int

instance AnytoneRender DataACKDisable where
  render (DataACKDisable i) = anytoneQuote $ show i

data R5ToneBot = R5ToneBot Int

instance AnytoneRender R5ToneBot where
  render (R5ToneBot i) = anytoneQuote $ show i

data R5ToneEot = R5ToneEot Int

instance AnytoneRender R5ToneEot where
  render (R5ToneEot i) = anytoneQuote $ show i

data AutoScan = AutoScan Int

instance AnytoneRender AutoScan where
  render (AutoScan i) = anytoneQuote $ show i

data AnaAPRSMute = AnaAPRSMute Int

instance AnytoneRender AnaAPRSMute where
  render (AnaAPRSMute i) = anytoneQuote $ show i

data SendTalkerAlias = SendTalkerAlias Int

instance AnytoneRender SendTalkerAlias where
  render (SendTalkerAlias i) = anytoneQuote $ show i

data AnaAPRSTxPath = AnaAPRSTxPath Int

instance AnytoneRender AnaAPRSTxPath where
  render (AnaAPRSTxPath i) = anytoneQuote $ show i

data ARC4 = ARC4 Int

instance AnytoneRender ARC4 where
  render (ARC4 i) = anytoneQuote $ show i

data ExEmgKind = ExEmgKind Int

instance AnytoneRender ExEmgKind where
  render (ExEmgKind i) = anytoneQuote $ show i

data Channel = Channel
  { no :: Int,
    name :: String,
    rxfreq :: Freq,
    txfreq :: Freq,
    channeltype :: ChannelType,
    txpower :: TxPower,
    bandwith :: BandWidth,
    ctcss_dcs_decode :: CTCSS_DCS,
    ctcss_dcs_encode :: CTCSS_DCS,
    contact :: Contact,
    contact_calltype :: ContactCallType,
    contact_tg_dmrid :: Int,
    radioid :: String,
    busylock :: BusyLock,
    squelch_mode :: SquelchMode,
    optionalSignal :: OptionalSignal,
    dtmfid :: DtmfId,
    twoToneId :: TwoToneId,
    fiveToneId :: FiveToneId,
    pttId :: PttId,
    colorcode :: ColorCode,
    slot :: Slot,
    scanlist :: Scanlist,
    receiveGroupList :: ReceiveGroupList,
    pttprohibit :: PttProhibit,
    reverse :: Reverse,
    simplexTDMA :: SimplexTDMA,
    slotSuit :: SlotSuit,
    aesDigitalEncryption :: AESDigiEncryption,
    digitalEncryption :: DigitalEncryption,
    callConfirmation :: CallConfirmation,
    talkAroundSimplex :: TalkAroundSimplex,
    workAlone :: WorkAlone,
    customCTCSS :: CustomCTCSS,
    twoToneDecode :: TwoToneDecode,
    ranging :: Ranging,
    throughMode :: ThroughMode,
    aprsRx :: APRSRx,
    analogAPRSPttMode :: AnalogAPRSPttMode,
    digitalAPRSPttMode :: DigitalAPRSPttMode,
    aprsReportType :: AprsReportType,
    digitalAPRSReportChannel :: DigitalAPRSReportChannel,
    correctFreq :: CorrectFreq,
    smsConfirmation :: SmsConfirmation,
    excludeChannelFromRoaming :: ExcludeChannelFromRoaming,
    dmrMode :: DMRMode,
    dataACKDisable :: DataACKDisable,
    r5ToneBot :: R5ToneBot,
    r5ToneEot :: R5ToneEot,
    autoScan :: AutoScan,
    anaAPRSMute :: AnaAPRSMute,
    sendTalkerAlias :: SendTalkerAlias,
    anaAPRSTxPath :: AnaAPRSTxPath,
    arc4 :: ARC4,
    exEmgKind :: ExEmgKind
  }

renderChannel :: Channel -> String
renderChannel c =
  intercalate
    ","
    ( map
        (\f -> f c)
        [ render . no,
          render . name,
          render . rxfreq,
          render . txfreq,
          render . channeltype,
          render . txpower,
          render . bandwith,
          render . ctcss_dcs_decode,
          render . ctcss_dcs_encode,
          render . contact,
          render . contact_calltype,
          render . contact_tg_dmrid,
          render . radioid,
          render . busylock,
          render . squelch_mode,
          render . optionalSignal,
          render . dtmfid,
          render . twoToneId,
          render . fiveToneId,
          render . pttId,
          render . colorcode,
          render . slot,
          render . scanlist,
          render . receiveGroupList,
          render . pttprohibit,
          render . reverse,
          render . simplexTDMA,
          render . slotSuit,
          render . aesDigitalEncryption,
          render . digitalEncryption,
          render . callConfirmation,
          render . talkAroundSimplex,
          render . workAlone,
          render . customCTCSS,
          render . twoToneDecode,
          render . ranging,
          render . throughMode,
          render . aprsRx,
          render . analogAPRSPttMode,
          render . digitalAPRSPttMode,
          render . aprsReportType,
          render . digitalAPRSReportChannel,
          render . correctFreq,
          render . smsConfirmation,
          render . excludeChannelFromRoaming,
          render . dmrMode,
          render . dataACKDisable,
          render . r5ToneBot,
          render . r5ToneEot,
          render . autoScan,
          render . anaAPRSMute,
          render . sendTalkerAlias,
          render . anaAPRSTxPath,
          render . arc4,
          render . exEmgKind
        ]
    )

defaultChannel :: Channel
defaultChannel =
  Channel
    0
    "default"
    (Freq 145.50000)
    (Freq 145.50000)
    Analog
    Low
    BW_12_5
    ToneOff
    ToneOff
    Local
    GroupCall
    1
    "NOCALL"
    BusyLockOff
    Carrier
    OptionalSignalOff
    DtmfIdOff
    (TwoToneId 1)
    (FiveToneId 1)
    PttIdOff
    (ColorCode 1)
    (Slot 1)
    ScanlistNone
    ReceiveGroupListNone
    PttProhibitOff
    ReverseOff
    SimplexTDMAOff
    SlotSuitOff
    NormalEncryption
    DigitalEncryptionOff
    CallConfirmationOff
    TalkAroundSimplexOff
    WorkAloneOff
    (CustomCTCSS $ Tone 251.1)
    (TwoToneDecode 1)
    RangingOff
    ThroughModeOn
    APRSRxOff
    AnalogAPRSPttModeOff
    DigitalAPRSPttModeOff
    AprsReportTypeOff
    (DigitalAPRSReportChannel 1)
    (CorrectFreq 0)
    SmsConfirmationOff
    (ExcludeChannelFromRoaming 1)
    (DMRMode 0)
    (DataACKDisable 0)
    (R5ToneBot 0)
    (R5ToneEot 0)
    (AutoScan 0)
    (AnaAPRSMute 0)
    (SendTalkerAlias 0)
    (AnaAPRSTxPath 0)
    (ARC4 0)
    (ExEmgKind 0)

csvHeader :: String
csvHeader =
  intercalate "," $
    map
      anytoneQuote
      [ "No.",
        "Channel Name",
        "Receive Frequency",
        "Transmit Frequency",
        "Channel Type",
        "Transmit Power",
        "Band Width",
        "CTCSS/DCS Decode",
        "CTCSS/DCS Encode",
        "Contact",
        "Contact Call Type",
        "Contact TG/DMR ID",
        "Radio ID",
        "Busy Lock/TX Permit",
        "Squelch Mode",
        "Optional Signal",
        "DTMF ID",
        "2Tone ID",
        "5Tone ID",
        "PTT ID",
        "Color Code",
        "Slot",
        "Scan List",
        "Receive Group List",
        "PTT Prohibit",
        "Reverse",
        "Simplex TDMA",
        "Slot Suit",
        "AES Digital Encryption",
        "Digital Encryption",
        "Call Confirmation",
        "Talk Around(Simplex)",
        "Work Alone",
        "Custom CTCSS",
        "2TONE Decode",
        "Ranging",
        "Through Mode",
        "APRS RX",
        "Analog APRS PTT Mode",
        "Digital APRS PTT Mode",
        "APRS Report Type",
        "Digital APRS Report Channel",
        "Correct Frequency[Hz]",
        "SMS Confirmation",
        "Exclude channel from roaming",
        "DMR MODE",
        "DataACK Disable",
        "R5toneBot",
        "R5ToneEot",
        "Auto Scan",
        "Ana Aprs Mute",
        "Send Talker Alias",
        "AnaAprsTxPath",
        "ARC4",
        "ex_emg_kind"
      ]

-- template_2m_simplex = "150","FM 2m Calling","145.50000","145.50000","A-Analog","Turbo",
--          "12.5K","Off","Off","Local","Group Call","1",
--          "DC1MDP","Off","Carrier","Off","1","1","1","Off",
--          "1","1","None","None","Off","Off","Off","Off",
--          "Normal Encryption","Off","Off","Off","Off",
--          "251.1","0","Off","On","Off","Off","Off",
--          "Off","1","0","Off",
--          "1","0","0", "0","0","0","0",
--          "0","0","0","0"

outputChannelsCSV :: Int -> [Channel] -> String
outputChannelsCSV n cs = csvHeader ++ "\n" ++ intercalate "\n" (zipWith (\c nn -> renderChannel $ c {no = nn}) cs [n ..])

fm2m144SimplexAnonChannels :: [Channel]
fm2m144SimplexAnonChannels = mapMaybe genChannel (zip [Freq f | f <- filter (\x -> x < 144.625 || x >= 144.7) [144.500, 144.5125 .. 144.7875 :: Micro]] [1 ..])
  where
    genChannel :: (Freq, Int) -> Maybe Channel
    genChannel (f, i) = case f of
      144.500 -> Just $ noTx f $ show i ++ " Anon SSTV Call"
      144.525 -> Nothing
      144.600 -> Just $ noTx f $ show i ++ " Anon RTTY Call"
      144.7 -> Just $ noTx f $ show i ++ " Anon FAX Call"
      144.75 -> Just $ noTx f $ show i ++ " Anon ATV recall"
      _ -> Just $ tx f $ show i ++ " Anon 144"
    noTx i n = defaultChannel {txfreq = i, rxfreq = i, pttprohibit = PttProhibitOn, name = n}
    tx i n = defaultChannel {txpower = High, txfreq = i, rxfreq = i, name = n}

fm2mSimplexChannels :: [Channel]
fm2mSimplexChannels = [genChannel i | i <- [16 :: Int .. 47]]
  where
    genChannel :: Int -> Channel
    genChannel i = case i of
      16 -> noTx (calcSimplexChannelFreq i) "V16 SatCom"
      24 -> noTx (calcSimplexChannelFreq i) "V24 RTTY local"
      40 -> tx (calcSimplexChannelFreq i) "V40 2m Call"
      _ -> tx (calcSimplexChannelFreq i) ("V" ++ show i)
    noTx i n = defaultChannel {txfreq = i, rxfreq = i, pttprohibit = PttProhibitOn, name = n}
    tx i n = defaultChannel {txpower = High, txfreq = i, rxfreq = i, name = n}
    calcSimplexChannelFreq :: Int -> Freq
    calcSimplexChannelFreq i = Freq 145 + fromIntegral i * 0.0125

fm2mRepeaters :: [Channel]
fm2mRepeaters = [genChannel x | i <- [48 .. 63], let r = (i - 48) `quot` 2, let x = (i, r)]
  where
    genChannel :: (Int, Int) -> Channel
    genChannel (i, r) =
      let f = Freq 145 + fromIntegral i * 0.0125
          n = if even i then "V" ++ show i ++ " R" ++ show r else "V" ++ show i ++ " R" ++ show r ++ "x"
          tx freq name = defaultChannel {txpower = High, txfreq = freq - 0.6, rxfreq = freq, name = name}
       in tx f n

fm70cmSimplexChannels :: [Channel]
fm70cmSimplexChannels = [genChannel i | i <- [1 :: Int .. 29] ++ [232 .. 319]]
  where
    genChannel :: Int -> Channel
    genChannel i = case i of
      2 -> noTx (calcSimplexChannelFreq i) "U002 Echlinik Simplx"
      4 -> noTx (calcSimplexChannelFreq i) "U004 Echlinik Simplx"
      30 -> noTx (calcSimplexChannelFreq i) "RU30 DVR"
      31 -> noTx (calcSimplexChannelFreq i) "RU31 DVR"
      272 -> tx (calcSimplexChannelFreq i) "U272 SSTV"
      280 -> tx (calcSimplexChannelFreq i) "U280 70cm Calling"
      288 -> tx (calcSimplexChannelFreq i) "U288 RTTY"
      296 -> noTx (calcSimplexChannelFreq i) "U296 FAX"
      _ -> tx (calcSimplexChannelFreq i) ("U" ++ printf "%03d" i)
    noTx i n = defaultChannel {txfreq = i, rxfreq = i, pttprohibit = PttProhibitOn, name = n}
    tx i n = defaultChannel {txpower = High, txfreq = i, rxfreq = i, name = n}
    calcSimplexChannelFreq :: Int -> Freq
    calcSimplexChannelFreq i = Freq 430 + fromIntegral i * 0.0125

fm70cmRepeaters :: [Channel]
fm70cmRepeaters = [genChannel x | i <- [692 .. 755], let r = 35 + (i - 622) `quot` 2, let x = (i, r)]
  where
    genChannel :: (Int, Int) -> Channel
    genChannel (i, r) =
      let f = Freq 430 + fromIntegral i * 0.0125
          n = if even i then "RU" ++ show i ++ " R" ++ show r else "RU" ++ show i ++ " R" ++ show r ++ "x"
          tx freq name = defaultChannel {txpower = High, txfreq = freq - 7.6, rxfreq = freq, name = name}
       in tx f n

fmFreenetSimplexChannels :: [Channel]
fmFreenetSimplexChannels = [genChannel i | i <- [1 :: Int .. 6]]
  where
    genChannel :: Int -> Channel
    genChannel i = tx (calcSimplexChannelFreq i) ("Freenet Ch " ++ show i)
    tx i n = defaultChannel {txpower = Low, txfreq = i, rxfreq = i, name = n}
    calcSimplexChannelFreq :: Int -> Freq
    calcSimplexChannelFreq i
      | i <= 3 = Freq 149.0125 + fromIntegral i * 0.0125
      | i > 3 = Freq 149.0750 + fromIntegral (i - 3) * 0.0125
      | otherwise = error "Invalid Freenet Channel number"

fmPMR446SimplexChannels :: [Channel]
fmPMR446SimplexChannels = [genChannel i | i <- [1 :: Int .. 16]]
  where
    genChannel :: Int -> Channel
    genChannel i = tx (calcSimplexChannelFreq i) ("PMR446 Ch " ++ show i)
    tx i n = defaultChannel {txpower = Low, txfreq = i, rxfreq = i, name = n}
    calcSimplexChannelFreq :: Int -> Freq
    calcSimplexChannelFreq i = Freq 445.99375 + fromIntegral i * 0.0125

data Args = Opts Options | All

parserArgs :: Parser (Args, Int)
parserArgs = do
  args <- options <|> parseAll
  n <- parseChannelNumber
  return (args, n)

parseChannelNumber :: Parser Int
parseChannelNumber =
  option
    auto
    ( long "channel-number"
        <> short 'n'
        <> help "Channelnumber to start, should be above the latest of your programmed channels"
        <> showDefault
        <> value 2000
        <> metavar "INT"
    )

data Options = Options
  { s70 :: Bool,
    r70 :: Bool,
    s2 :: Bool,
    r2 :: Bool,
    pmr446 :: Bool,
    freenet :: Bool,
    sat :: Bool
  }

options :: Parser Args
options =
  Opts
    <$> ( Options
            <$> switch
              ( long "70cm"
                  <> short '7'
                  <> help "generate 70cm fm simplex channels"
              )
            <*> switch
              ( long "r70cm"
                  <> short 'r'
                  <> help "generate 70cm fm repeater channels"
              )
            <*> switch
              ( long "2m"
                  <> short '2'
                  <> help "generate 2cm fm simplex channels"
              )
            <*> switch
              ( long "r2m"
                  <> short 'R'
                  <> help "generate 2m fm repeater channels"
              )
            <*> switch
              ( long "pmr446"
                  <> short '4'
                  <> help "generate pmr fm simplex channels"
              )
            <*> switch
              ( long "freenet"
                  <> short 'f'
                  <> help "generate pmr fm simplex channels"
              )
            <*> switch
              ( long "sat"
                  <> short 's'
                  <> help "generate fm sat tle file (./anytool.tle)"
              )
        )

parseAll :: Parser Args
parseAll =
  All
    <$ switch
      ( long "all"
          <> short 'a'
          <> help "generate all data"
      )

main :: IO ()
main = go =<< execParser opts
  where
    opts =
      info
        (parserArgs <**> helper)
        ( fullDesc
            <> Options.Applicative.header "DC1MDP Maurizio Di Pietro © 2024-- Use as is. I take no responsibility for any inconvieniences you may encouter using this tool."
            <> progDesc "Anytool -- Utility to generate various channel and satellite data for the Anytone AT-D878UVIIplus"
        )
    go :: (Args, Int) -> IO ()
    go (a, n) = case a of
      All ->
        putStrLn $
          outputChannelsCSV n $
            fm2m144SimplexAnonChannels --
              ++ fm2mSimplexChannels
              ++ fm2mRepeaters
              ++ fm70cmSimplexChannels
              ++ fm70cmRepeaters
              ++ fmPMR446SimplexChannels
              ++ fmFreenetSimplexChannels
      Opts o -> do
        let f2s = bool [] (fm2mSimplexChannels ++ fm2m144SimplexAnonChannels) (s2 o)
            f2r = bool [] fm2mRepeaters (r2 o)
            f7s = bool [] fm70cmSimplexChannels (s70 o)
            f7r = bool [] fm70cmRepeaters (r70 o)
            pmr = bool [] fmPMR446SimplexChannels (pmr446 o)
            fnet = bool [] fmFreenetSimplexChannels (freenet o)
        --     putStrLn $
        --       outputChannelsCSV n $
        --         f2s ++ f2r ++ f7s ++ f7r ++ pmr ++ fnet
        writeTLEFile (sat o)

type SatID = Int

satids :: [SatID]
satids =
  [ 25544, -- ISS
    22825, -- AO-27
    24278, -- FO-29
    27607, -- SO-50
    40967, -- AO-85 (FOX-1A)
    40908 -- LILACSAT-2
  ]

writeTLEFile :: Bool -> IO ()
writeTLEFile False = pure ()
writeTLEFile _ = do
  tles <- mapM downloadTLE satids
  let outp = B.concat tles
  withFile "anytool.tle.txt" WriteMode $ \h -> do
    B.hPutStr h outp

downloadTLE :: Int -> IO B.ByteString
downloadTLE i = runReq defaultHttpConfig $ do
  bs <-
    req GET (http "celestrak.org" /: "NORAD" /: "elements" /: "gp.php") NoReqBody bsResponse $
      "CATNR" =: i
        <> "FORMAT" =: ("tle" :: String)
  pure $ responseBody bs

-- "http://celestrak.org/NORAD/elements/gp.php?CATNR=" ++ satid ++ "&FORMAT=tle"
