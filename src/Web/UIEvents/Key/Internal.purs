module Web.UIEvents.Key.Internal where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String as S
import Web.UIEvents.Key.Internal.Category (Category(..)) as Category
import Web.UIEvents.Key.Internal.Category (Category())
import Web.UIEvents.Key.Internal.Modifier (Modifier(..)) as Modifier
import Web.UIEvents.Key.Internal.Modifier (Modifier())

data Key
  = Unicode String
  | Unidentified
  | Alt
  | AltGraph
  | CapsLock
  | Control
  | Fn
  | FnLock
  | Meta
  | NumLock
  | ScrollLock
  | Shift
  | Symbol
  | SymbolLock
  | Hyper
  | Super
  | Enter
  | Tab
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | End
  | Home
  | PageDown
  | PageUp
  | Backspace
  | Clear
  | Copy
  | CrSel
  | Cut
  | Delete
  | EraseEof
  | ExSel
  | Insert
  | Paste
  | Redo
  | Undo
  | Accept
  | Again
  | Attn
  | Cancel
  | ContextMenu
  | Escape
  | Execute
  | Find
  | Help
  | Pause
  | Play
  | Props
  | Select
  | ZoomIn
  | ZoomOut
  | BrightnessDown
  | BrightnessUp
  | Eject
  | LogOff
  | Power
  | PowerOff
  | PrintScreen
  | Hibernate
  | Standby
  | WakeUp
  | AllCandidates
  | Alphanumeric
  | CodeInput
  | Compose
  | Convert
  | Dead
  | FinalMode
  | GroupFirst
  | GroupLast
  | GroupNext
  | GroupPrevious
  | ModeChange
  | NextCandidate
  | NonConvert
  | PreviousCandidate
  | Process
  | SingleCandidate
  | HangulMode
  | HanjaMode
  | JunjaMode
  | Eisu
  | Hankaku
  | Hiragana
  | HiraganaKatakana
  | KanaMode
  | KanjiMode
  | Katakana
  | Romaji
  | Zenkaku
  | ZenkakuHankaku
  | F Int
  | Soft Int
  | ChannelDown
  | ChannelUp
  | Close
  | MailForward
  | MailReply
  | MailSend
  | MediaClose
  | MediaFastForward
  | MediaPause
  | MediaPlay
  | MediaPlayPause
  | MediaRecord
  | MediaRewind
  | MediaStop
  | MediaTrackNext
  | MediaTrackPrevious
  | New
  | Open
  | Print
  | Save
  | SpellCheck
  | Key11
  | Key12
  | AudioBalanceLeft
  | AudioBalanceRight
  | AudioBassBoostDown
  | AudioBassBoostToggle
  | AudioBassBoostUp
  | AudioFaderFront
  | AudioFaderRear
  | AudioSurroundModeNext
  | AudioTrebleDown
  | AudioTrebleUp
  | AudioVolumeDown
  | AudioVolumeUp
  | AudioVolumeMute
  | MicrophoneToggle
  | MicrophoneVolumeDown
  | MicrophoneVolumeUp
  | MicrophoneVolumeMute
  | SpeechCorrectionList
  | SpeechInputToggle
  | LaunchApplication Int
  | LaunchCalendar
  | LaunchContacts
  | LaunchMail
  | LaunchMediaPlayer
  | LaunchMusicPlayer
  | LaunchPhone
  | LaunchScreenSaver
  | LaunchSpreadsheet
  | LaunchWebBrowser
  | LaunchWebCam
  | LaunchWordProcessor
  | BrowserBack
  | BrowserFavorites
  | BrowserForward
  | BrowserHome
  | BrowserRefresh
  | BrowserSearch
  | BrowserStop
  | AppSwitch
  | Call
  | Camera
  | CameraFocus
  | EndCall
  | GoBack
  | GoHome
  | HeadsetHook
  | LastNumberRedial
  | Notification
  | MannerMode
  | VoiceDial
  | TV
  | TV3DMode
  | TVAntennaCable
  | TVAudioDescription
  | TVAudioDescriptionMixDown
  | TVAudioDescriptionMixUp
  | TVContentsMenu
  | TVDataService
  | TVInput
  | TVInputComponent Int
  | TVInputComposite Int
  | TVInputHDMI Int
  | TVInputVGA1
  | TVMediaContext
  | TVNetwork
  | TVNumberEntry
  | TVPower
  | TVRadioService
  | TVSatellite
  | TVSatelliteBS
  | TVSatelliteCS
  | TVSatelliteToggle
  | TVTerrestrialAnalog
  | TVTerrestrialDigital
  | TVTimer
  | AVRInput
  | AVRPower
  | ColorF0Red
  | ColorF1Green
  | ColorF2Yellow
  | ColorF3Blue
  | ColorF4Grey
  | ColorF5Brown
  | ClosedCaptionToggle
  | Dimmer
  | DisplaySwap
  | DVR
  | Exit
  | FavoriteClear Int
  | FavoriteRecall Int
  | FavoriteStore Int
  | Guide
  | GuideNextDay
  | GuidePreviousDay
  | Info
  | InstantReplay
  | Link
  | ListProgram
  | LiveContent
  | Lock
  | MediaApps
  | MediaAudioTrack
  | MediaLast
  | MediaSkipBackward
  | MediaSkipForward
  | MediaStepBackward
  | MediaStepForward
  | MediaTopMenu
  | NavigateIn
  | NavigateNext
  | NavigateOut
  | NavigatePrevious
  | NextFavoriteChannel
  | NextUserProfile
  | OnDemand
  | Pairing
  | PinPDown
  | PinPMove
  | PinPToggle
  | PinPUp
  | PlaySpeedDown
  | PlaySpeedReset
  | PlaySpeedUp
  | RandomToggle
  | RcLowBattery
  | RecordSpeedNext
  | RfBypass
  | ScanChannelsToggle
  | ScreenModeNext
  | Settings
  | SplitScreenToggle
  | STBInput
  | STBPower
  | Subtitle
  | Teletext
  | VideoModeNext
  | Wink
  | ZoomToggle
derive instance eqKey :: Eq Key
derive instance ordKey :: Ord Key

tryParse :: String -> String -> Maybe Int
tryParse prefix = fromString <=< S.stripPrefix (S.Pattern prefix)

category :: Key -> Category
category (Unicode _) = Category.Character
category Unidentified = Category.Special
category Alt = Category.Modifier
category AltGraph = Category.Modifier
category CapsLock = Category.Modifier
category Control = Category.Modifier
category Fn = Category.Modifier
category FnLock = Category.Modifier
category Meta = Category.Modifier
category NumLock = Category.Modifier
category ScrollLock = Category.Modifier
category Shift = Category.Modifier
category Symbol = Category.Modifier
category SymbolLock = Category.Modifier
category Hyper = Category.Modifier
category Super = Category.Modifier
category Enter = Category.Whitespace
category Tab = Category.Whitespace
category ArrowDown = Category.Navigation
category ArrowLeft = Category.Navigation
category ArrowRight = Category.Navigation
category ArrowUp = Category.Navigation
category End = Category.Navigation
category Home = Category.Navigation
category PageDown = Category.Navigation
category PageUp = Category.Navigation
category Backspace = Category.Editing
category Clear = Category.Editing
category Copy = Category.Editing
category CrSel = Category.Editing
category Cut = Category.Editing
category Delete = Category.Editing
category EraseEof = Category.Editing
category ExSel = Category.Editing
category Insert = Category.Editing
category Paste = Category.Editing
category Redo = Category.Editing
category Undo = Category.Editing
category Accept = Category.UI
category Again = Category.UI
category Attn = Category.UI
category Cancel = Category.UI
category ContextMenu = Category.UI
category Escape = Category.UI
category Execute = Category.UI
category Find = Category.UI
category Help = Category.UI
category Pause = Category.UI
category Play = Category.UI
category Props = Category.UI
category Select = Category.UI
category ZoomIn = Category.UI
category ZoomOut = Category.UI
category BrightnessDown = Category.Device
category BrightnessUp = Category.Device
category Eject = Category.Device
category LogOff = Category.Device
category Power = Category.Device
category PowerOff = Category.Device
category PrintScreen = Category.Device
category Hibernate = Category.Device
category Standby = Category.Device
category WakeUp = Category.Device
category AllCandidates = Category.Composition
category Alphanumeric = Category.Composition
category CodeInput = Category.Composition
category Compose = Category.Composition
category Convert = Category.Composition
category Dead = Category.Composition
category FinalMode = Category.Composition
category GroupFirst = Category.Composition
category GroupLast = Category.Composition
category GroupNext = Category.Composition
category GroupPrevious = Category.Composition
category ModeChange = Category.Composition
category NextCandidate = Category.Composition
category NonConvert = Category.Composition
category PreviousCandidate = Category.Composition
category Process = Category.Composition
category SingleCandidate = Category.Composition
category HangulMode = Category.Composition
category HanjaMode = Category.Composition
category JunjaMode = Category.Composition
category Eisu = Category.Composition
category Hankaku = Category.Composition
category Hiragana = Category.Composition
category HiraganaKatakana = Category.Composition
category KanaMode = Category.Composition
category KanjiMode = Category.Composition
category Katakana = Category.Composition
category Romaji = Category.Composition
category Zenkaku = Category.Composition
category ZenkakuHankaku = Category.Composition
category (F _) = Category.Function
category (Soft _) = Category.Function
category ChannelDown = Category.Multimedia
category ChannelUp = Category.Multimedia
category Close = Category.Multimedia
category MailForward = Category.Multimedia
category MailReply = Category.Multimedia
category MailSend = Category.Multimedia
category MediaClose = Category.Multimedia
category MediaFastForward = Category.Multimedia
category MediaPause = Category.Multimedia
category MediaPlay = Category.Multimedia
category MediaPlayPause = Category.Multimedia
category MediaRecord = Category.Multimedia
category MediaRewind = Category.Multimedia
category MediaStop = Category.Multimedia
category MediaTrackNext = Category.Multimedia
category MediaTrackPrevious = Category.Multimedia
category New = Category.Multimedia
category Open = Category.Multimedia
category Print = Category.Multimedia
category Save = Category.Multimedia
category SpellCheck = Category.Multimedia
category Key11 = Category.MultimediaNumpad
category Key12 = Category.MultimediaNumpad
category AudioBalanceLeft = Category.Audio
category AudioBalanceRight = Category.Audio
category AudioBassBoostDown = Category.Audio
category AudioBassBoostToggle = Category.Audio
category AudioBassBoostUp = Category.Audio
category AudioFaderFront = Category.Audio
category AudioFaderRear = Category.Audio
category AudioSurroundModeNext = Category.Audio
category AudioTrebleDown = Category.Audio
category AudioTrebleUp = Category.Audio
category AudioVolumeDown = Category.Audio
category AudioVolumeUp = Category.Audio
category AudioVolumeMute = Category.Audio
category MicrophoneToggle = Category.Audio
category MicrophoneVolumeDown = Category.Audio
category MicrophoneVolumeUp = Category.Audio
category MicrophoneVolumeMute = Category.Audio
category SpeechCorrectionList = Category.Speech
category SpeechInputToggle = Category.Speech
category (LaunchApplication _) = Category.Application
category LaunchCalendar = Category.Application
category LaunchContacts = Category.Application
category LaunchMail = Category.Application
category LaunchMediaPlayer = Category.Application
category LaunchMusicPlayer = Category.Application
category LaunchPhone = Category.Application
category LaunchScreenSaver = Category.Application
category LaunchSpreadsheet = Category.Application
category LaunchWebBrowser = Category.Application
category LaunchWebCam = Category.Application
category LaunchWordProcessor = Category.Application
category BrowserBack = Category.Browser
category BrowserFavorites = Category.Browser
category BrowserForward = Category.Browser
category BrowserHome = Category.Browser
category BrowserRefresh = Category.Browser
category BrowserSearch = Category.Browser
category BrowserStop = Category.Browser
category AppSwitch = Category.MobilePhone
category Call = Category.MobilePhone
category Camera = Category.MobilePhone
category CameraFocus = Category.MobilePhone
category EndCall = Category.MobilePhone
category GoBack = Category.MobilePhone
category GoHome = Category.MobilePhone
category HeadsetHook = Category.MobilePhone
category LastNumberRedial = Category.MobilePhone
category Notification = Category.MobilePhone
category MannerMode = Category.MobilePhone
category VoiceDial = Category.MobilePhone
category TV = Category.TV
category TV3DMode = Category.TV
category TVAntennaCable = Category.TV
category TVAudioDescription = Category.TV
category TVAudioDescriptionMixDown = Category.TV
category TVAudioDescriptionMixUp = Category.TV
category TVContentsMenu = Category.TV
category TVDataService = Category.TV
category TVInput = Category.TV
category (TVInputComponent _) = Category.TV
category (TVInputComposite _) = Category.TV
category (TVInputHDMI _) = Category.TV
category TVInputVGA1 = Category.TV
category TVMediaContext = Category.TV
category TVNetwork = Category.TV
category TVNumberEntry = Category.TV
category TVPower = Category.TV
category TVRadioService = Category.TV
category TVSatellite = Category.TV
category TVSatelliteBS = Category.TV
category TVSatelliteCS = Category.TV
category TVSatelliteToggle = Category.TV
category TVTerrestrialAnalog = Category.TV
category TVTerrestrialDigital = Category.TV
category TVTimer = Category.TV
category AVRInput = Category.MediaController
category AVRPower = Category.MediaController
category ColorF0Red = Category.MediaController
category ColorF1Green = Category.MediaController
category ColorF2Yellow = Category.MediaController
category ColorF3Blue = Category.MediaController
category ColorF4Grey = Category.MediaController
category ColorF5Brown = Category.MediaController
category ClosedCaptionToggle = Category.MediaController
category Dimmer = Category.MediaController
category DisplaySwap = Category.MediaController
category DVR = Category.MediaController
category Exit = Category.MediaController
category (FavoriteClear _) = Category.MediaController
category (FavoriteRecall _) = Category.MediaController
category (FavoriteStore _) = Category.MediaController
category Guide = Category.MediaController
category GuideNextDay = Category.MediaController
category GuidePreviousDay = Category.MediaController
category Info = Category.MediaController
category InstantReplay = Category.MediaController
category Link = Category.MediaController
category ListProgram = Category.MediaController
category LiveContent = Category.MediaController
category Lock = Category.MediaController
category MediaApps = Category.MediaController
category MediaAudioTrack = Category.MediaController
category MediaLast = Category.MediaController
category MediaSkipBackward = Category.MediaController
category MediaSkipForward = Category.MediaController
category MediaStepBackward = Category.MediaController
category MediaStepForward = Category.MediaController
category MediaTopMenu = Category.MediaController
category NavigateIn = Category.MediaController
category NavigateNext = Category.MediaController
category NavigateOut = Category.MediaController
category NavigatePrevious = Category.MediaController
category NextFavoriteChannel = Category.MediaController
category NextUserProfile = Category.MediaController
category OnDemand = Category.MediaController
category Pairing = Category.MediaController
category PinPDown = Category.MediaController
category PinPMove = Category.MediaController
category PinPToggle = Category.MediaController
category PinPUp = Category.MediaController
category PlaySpeedDown = Category.MediaController
category PlaySpeedReset = Category.MediaController
category PlaySpeedUp = Category.MediaController
category RandomToggle = Category.MediaController
category RcLowBattery = Category.MediaController
category RecordSpeedNext = Category.MediaController
category RfBypass = Category.MediaController
category ScanChannelsToggle = Category.MediaController
category ScreenModeNext = Category.MediaController
category Settings = Category.MediaController
category SplitScreenToggle = Category.MediaController
category STBInput = Category.MediaController
category STBPower = Category.MediaController
category Subtitle = Category.MediaController
category Teletext = Category.MediaController
category VideoModeNext = Category.MediaController
category Wink = Category.MediaController
category ZoomToggle = Category.MediaController

parseImpl :: String -> Maybe Key
parseImpl "Unidentified" = Just Unidentified
parseImpl "Alt" = Just Alt
parseImpl "AltGraph" = Just AltGraph
parseImpl "CapsLock" = Just CapsLock
parseImpl "Control" = Just Control
parseImpl "Fn" = Just Fn
parseImpl "FnLock" = Just FnLock
parseImpl "Meta" = Just Meta
parseImpl "NumLock" = Just NumLock
parseImpl "ScrollLock" = Just ScrollLock
parseImpl "Shift" = Just Shift
parseImpl "Symbol" = Just Symbol
parseImpl "SymbolLock" = Just SymbolLock
parseImpl "Hyper" = Just Hyper
parseImpl "Super" = Just Super
parseImpl "Enter" = Just Enter
parseImpl "Tab" = Just Tab
parseImpl "ArrowDown" = Just ArrowDown
parseImpl "ArrowLeft" = Just ArrowLeft
parseImpl "ArrowRight" = Just ArrowRight
parseImpl "ArrowUp" = Just ArrowUp
parseImpl "End" = Just End
parseImpl "Home" = Just Home
parseImpl "PageDown" = Just PageDown
parseImpl "PageUp" = Just PageUp
parseImpl "Backspace" = Just Backspace
parseImpl "Clear" = Just Clear
parseImpl "Copy" = Just Copy
parseImpl "CrSel" = Just CrSel
parseImpl "Cut" = Just Cut
parseImpl "Delete" = Just Delete
parseImpl "EraseEof" = Just EraseEof
parseImpl "ExSel" = Just ExSel
parseImpl "Insert" = Just Insert
parseImpl "Paste" = Just Paste
parseImpl "Redo" = Just Redo
parseImpl "Undo" = Just Undo
parseImpl "Accept" = Just Accept
parseImpl "Again" = Just Again
parseImpl "Attn" = Just Attn
parseImpl "Cancel" = Just Cancel
parseImpl "ContextMenu" = Just ContextMenu
parseImpl "Escape" = Just Escape
parseImpl "Execute" = Just Execute
parseImpl "Find" = Just Find
parseImpl "Help" = Just Help
parseImpl "Pause" = Just Pause
parseImpl "Play" = Just Play
parseImpl "Props" = Just Props
parseImpl "Select" = Just Select
parseImpl "ZoomIn" = Just ZoomIn
parseImpl "ZoomOut" = Just ZoomOut
parseImpl "BrightnessDown" = Just BrightnessDown
parseImpl "BrightnessUp" = Just BrightnessUp
parseImpl "Eject" = Just Eject
parseImpl "LogOff" = Just LogOff
parseImpl "Power" = Just Power
parseImpl "PowerOff" = Just PowerOff
parseImpl "PrintScreen" = Just PrintScreen
parseImpl "Hibernate" = Just Hibernate
parseImpl "Standby" = Just Standby
parseImpl "WakeUp" = Just WakeUp
parseImpl "AllCandidates" = Just AllCandidates
parseImpl "Alphanumeric" = Just Alphanumeric
parseImpl "CodeInput" = Just CodeInput
parseImpl "Compose" = Just Compose
parseImpl "Convert" = Just Convert
parseImpl "Dead" = Just Dead
parseImpl "FinalMode" = Just FinalMode
parseImpl "GroupFirst" = Just GroupFirst
parseImpl "GroupLast" = Just GroupLast
parseImpl "GroupNext" = Just GroupNext
parseImpl "GroupPrevious" = Just GroupPrevious
parseImpl "ModeChange" = Just ModeChange
parseImpl "NextCandidate" = Just NextCandidate
parseImpl "NonConvert" = Just NonConvert
parseImpl "PreviousCandidate" = Just PreviousCandidate
parseImpl "Process" = Just Process
parseImpl "SingleCandidate" = Just SingleCandidate
parseImpl "HangulMode" = Just HangulMode
parseImpl "HanjaMode" = Just HanjaMode
parseImpl "JunjaMode" = Just JunjaMode
parseImpl "Eisu" = Just Eisu
parseImpl "Hankaku" = Just Hankaku
parseImpl "Hiragana" = Just Hiragana
parseImpl "HiraganaKatakana" = Just HiraganaKatakana
parseImpl "KanaMode" = Just KanaMode
parseImpl "KanjiMode" = Just KanjiMode
parseImpl "Katakana" = Just Katakana
parseImpl "Romaji" = Just Romaji
parseImpl "Zenkaku" = Just Zenkaku
parseImpl "ZenkakuHankaku" = Just ZenkakuHankaku
parseImpl s | Just i <- tryParse "F" s = Just (F i)
parseImpl s | Just i <- tryParse "Soft" s = Just (Soft i)
parseImpl "ChannelDown" = Just ChannelDown
parseImpl "ChannelUp" = Just ChannelUp
parseImpl "Close" = Just Close
parseImpl "MailForward" = Just MailForward
parseImpl "MailReply" = Just MailReply
parseImpl "MailSend" = Just MailSend
parseImpl "MediaClose" = Just MediaClose
parseImpl "MediaFastForward" = Just MediaFastForward
parseImpl "MediaPause" = Just MediaPause
parseImpl "MediaPlay" = Just MediaPlay
parseImpl "MediaPlayPause" = Just MediaPlayPause
parseImpl "MediaRecord" = Just MediaRecord
parseImpl "MediaRewind" = Just MediaRewind
parseImpl "MediaStop" = Just MediaStop
parseImpl "MediaTrackNext" = Just MediaTrackNext
parseImpl "MediaTrackPrevious" = Just MediaTrackPrevious
parseImpl "New" = Just New
parseImpl "Open" = Just Open
parseImpl "Print" = Just Print
parseImpl "Save" = Just Save
parseImpl "SpellCheck" = Just SpellCheck
parseImpl "Key11" = Just Key11
parseImpl "Key12" = Just Key12
parseImpl "AudioBalanceLeft" = Just AudioBalanceLeft
parseImpl "AudioBalanceRight" = Just AudioBalanceRight
parseImpl "AudioBassBoostDown" = Just AudioBassBoostDown
parseImpl "AudioBassBoostToggle" = Just AudioBassBoostToggle
parseImpl "AudioBassBoostUp" = Just AudioBassBoostUp
parseImpl "AudioFaderFront" = Just AudioFaderFront
parseImpl "AudioFaderRear" = Just AudioFaderRear
parseImpl "AudioSurroundModeNext" = Just AudioSurroundModeNext
parseImpl "AudioTrebleDown" = Just AudioTrebleDown
parseImpl "AudioTrebleUp" = Just AudioTrebleUp
parseImpl "AudioVolumeDown" = Just AudioVolumeDown
parseImpl "AudioVolumeUp" = Just AudioVolumeUp
parseImpl "AudioVolumeMute" = Just AudioVolumeMute
parseImpl "MicrophoneToggle" = Just MicrophoneToggle
parseImpl "MicrophoneVolumeDown" = Just MicrophoneVolumeDown
parseImpl "MicrophoneVolumeUp" = Just MicrophoneVolumeUp
parseImpl "MicrophoneVolumeMute" = Just MicrophoneVolumeMute
parseImpl "SpeechCorrectionList" = Just SpeechCorrectionList
parseImpl "SpeechInputToggle" = Just SpeechInputToggle
parseImpl s | Just i <- tryParse "LaunchApplication" s = Just (LaunchApplication i)
parseImpl "LaunchCalendar" = Just LaunchCalendar
parseImpl "LaunchContacts" = Just LaunchContacts
parseImpl "LaunchMail" = Just LaunchMail
parseImpl "LaunchMediaPlayer" = Just LaunchMediaPlayer
parseImpl "LaunchMusicPlayer" = Just LaunchMusicPlayer
parseImpl "LaunchPhone" = Just LaunchPhone
parseImpl "LaunchScreenSaver" = Just LaunchScreenSaver
parseImpl "LaunchSpreadsheet" = Just LaunchSpreadsheet
parseImpl "LaunchWebBrowser" = Just LaunchWebBrowser
parseImpl "LaunchWebCam" = Just LaunchWebCam
parseImpl "LaunchWordProcessor" = Just LaunchWordProcessor
parseImpl "BrowserBack" = Just BrowserBack
parseImpl "BrowserFavorites" = Just BrowserFavorites
parseImpl "BrowserForward" = Just BrowserForward
parseImpl "BrowserHome" = Just BrowserHome
parseImpl "BrowserRefresh" = Just BrowserRefresh
parseImpl "BrowserSearch" = Just BrowserSearch
parseImpl "BrowserStop" = Just BrowserStop
parseImpl "AppSwitch" = Just AppSwitch
parseImpl "Call" = Just Call
parseImpl "Camera" = Just Camera
parseImpl "CameraFocus" = Just CameraFocus
parseImpl "EndCall" = Just EndCall
parseImpl "GoBack" = Just GoBack
parseImpl "GoHome" = Just GoHome
parseImpl "HeadsetHook" = Just HeadsetHook
parseImpl "LastNumberRedial" = Just LastNumberRedial
parseImpl "Notification" = Just Notification
parseImpl "MannerMode" = Just MannerMode
parseImpl "VoiceDial" = Just VoiceDial
parseImpl "TV" = Just TV
parseImpl "TV3DMode" = Just TV3DMode
parseImpl "TVAntennaCable" = Just TVAntennaCable
parseImpl "TVAudioDescription" = Just TVAudioDescription
parseImpl "TVAudioDescriptionMixDown" = Just TVAudioDescriptionMixDown
parseImpl "TVAudioDescriptionMixUp" = Just TVAudioDescriptionMixUp
parseImpl "TVContentsMenu" = Just TVContentsMenu
parseImpl "TVDataService" = Just TVDataService
parseImpl "TVInput" = Just TVInput
parseImpl s | Just i <- tryParse "TVInputComponent" s = Just (TVInputComponent i)
parseImpl s | Just i <- tryParse "TVInputComposite" s = Just (TVInputComposite i)
parseImpl s | Just i <- tryParse "TVInputHDMI" s = Just (TVInputHDMI i)
parseImpl "TVInputVGA1" = Just TVInputVGA1
parseImpl "TVMediaContext" = Just TVMediaContext
parseImpl "TVNetwork" = Just TVNetwork
parseImpl "TVNumberEntry" = Just TVNumberEntry
parseImpl "TVPower" = Just TVPower
parseImpl "TVRadioService" = Just TVRadioService
parseImpl "TVSatellite" = Just TVSatellite
parseImpl "TVSatelliteBS" = Just TVSatelliteBS
parseImpl "TVSatelliteCS" = Just TVSatelliteCS
parseImpl "TVSatelliteToggle" = Just TVSatelliteToggle
parseImpl "TVTerrestrialAnalog" = Just TVTerrestrialAnalog
parseImpl "TVTerrestrialDigital" = Just TVTerrestrialDigital
parseImpl "TVTimer" = Just TVTimer
parseImpl "AVRInput" = Just AVRInput
parseImpl "AVRPower" = Just AVRPower
parseImpl "ColorF0Red" = Just ColorF0Red
parseImpl "ColorF1Green" = Just ColorF1Green
parseImpl "ColorF2Yellow" = Just ColorF2Yellow
parseImpl "ColorF3Blue" = Just ColorF3Blue
parseImpl "ColorF4Grey" = Just ColorF4Grey
parseImpl "ColorF5Brown" = Just ColorF5Brown
parseImpl "ClosedCaptionToggle" = Just ClosedCaptionToggle
parseImpl "Dimmer" = Just Dimmer
parseImpl "DisplaySwap" = Just DisplaySwap
parseImpl "DVR" = Just DVR
parseImpl "Exit" = Just Exit
parseImpl s | Just i <- tryParse "FavoriteClear" s = Just (FavoriteClear i)
parseImpl s | Just i <- tryParse "FavoriteRecall" s = Just (FavoriteRecall i)
parseImpl s | Just i <- tryParse "FavoriteStore" s = Just (FavoriteStore i)
parseImpl "Guide" = Just Guide
parseImpl "GuideNextDay" = Just GuideNextDay
parseImpl "GuidePreviousDay" = Just GuidePreviousDay
parseImpl "Info" = Just Info
parseImpl "InstantReplay" = Just InstantReplay
parseImpl "Link" = Just Link
parseImpl "ListProgram" = Just ListProgram
parseImpl "LiveContent" = Just LiveContent
parseImpl "Lock" = Just Lock
parseImpl "MediaApps" = Just MediaApps
parseImpl "MediaAudioTrack" = Just MediaAudioTrack
parseImpl "MediaLast" = Just MediaLast
parseImpl "MediaSkipBackward" = Just MediaSkipBackward
parseImpl "MediaSkipForward" = Just MediaSkipForward
parseImpl "MediaStepBackward" = Just MediaStepBackward
parseImpl "MediaStepForward" = Just MediaStepForward
parseImpl "MediaTopMenu" = Just MediaTopMenu
parseImpl "NavigateIn" = Just NavigateIn
parseImpl "NavigateNext" = Just NavigateNext
parseImpl "NavigateOut" = Just NavigateOut
parseImpl "NavigatePrevious" = Just NavigatePrevious
parseImpl "NextFavoriteChannel" = Just NextFavoriteChannel
parseImpl "NextUserProfile" = Just NextUserProfile
parseImpl "OnDemand" = Just OnDemand
parseImpl "Pairing" = Just Pairing
parseImpl "PinPDown" = Just PinPDown
parseImpl "PinPMove" = Just PinPMove
parseImpl "PinPToggle" = Just PinPToggle
parseImpl "PinPUp" = Just PinPUp
parseImpl "PlaySpeedDown" = Just PlaySpeedDown
parseImpl "PlaySpeedReset" = Just PlaySpeedReset
parseImpl "PlaySpeedUp" = Just PlaySpeedUp
parseImpl "RandomToggle" = Just RandomToggle
parseImpl "RcLowBattery" = Just RcLowBattery
parseImpl "RecordSpeedNext" = Just RecordSpeedNext
parseImpl "RfBypass" = Just RfBypass
parseImpl "ScanChannelsToggle" = Just ScanChannelsToggle
parseImpl "ScreenModeNext" = Just ScreenModeNext
parseImpl "Settings" = Just Settings
parseImpl "SplitScreenToggle" = Just SplitScreenToggle
parseImpl "STBInput" = Just STBInput
parseImpl "STBPower" = Just STBPower
parseImpl "Subtitle" = Just Subtitle
parseImpl "Teletext" = Just Teletext
parseImpl "VideoModeNext" = Just VideoModeNext
parseImpl "Wink" = Just Wink
parseImpl "ZoomToggle" = Just ZoomToggle
parseImpl _ = Nothing

unparse :: Key -> String
unparse (Unicode c) = c
unparse Unidentified = "Unidentified"
unparse Alt = "Alt"
unparse AltGraph = "AltGraph"
unparse CapsLock = "CapsLock"
unparse Control = "Control"
unparse Fn = "Fn"
unparse FnLock = "FnLock"
unparse Meta = "Meta"
unparse NumLock = "NumLock"
unparse ScrollLock = "ScrollLock"
unparse Shift = "Shift"
unparse Symbol = "Symbol"
unparse SymbolLock = "SymbolLock"
unparse Hyper = "Hyper"
unparse Super = "Super"
unparse Enter = "Enter"
unparse Tab = "Tab"
unparse ArrowDown = "ArrowDown"
unparse ArrowLeft = "ArrowLeft"
unparse ArrowRight = "ArrowRight"
unparse ArrowUp = "ArrowUp"
unparse End = "End"
unparse Home = "Home"
unparse PageDown = "PageDown"
unparse PageUp = "PageUp"
unparse Backspace = "Backspace"
unparse Clear = "Clear"
unparse Copy = "Copy"
unparse CrSel = "CrSel"
unparse Cut = "Cut"
unparse Delete = "Delete"
unparse EraseEof = "EraseEof"
unparse ExSel = "ExSel"
unparse Insert = "Insert"
unparse Paste = "Paste"
unparse Redo = "Redo"
unparse Undo = "Undo"
unparse Accept = "Accept"
unparse Again = "Again"
unparse Attn = "Attn"
unparse Cancel = "Cancel"
unparse ContextMenu = "ContextMenu"
unparse Escape = "Escape"
unparse Execute = "Execute"
unparse Find = "Find"
unparse Help = "Help"
unparse Pause = "Pause"
unparse Play = "Play"
unparse Props = "Props"
unparse Select = "Select"
unparse ZoomIn = "ZoomIn"
unparse ZoomOut = "ZoomOut"
unparse BrightnessDown = "BrightnessDown"
unparse BrightnessUp = "BrightnessUp"
unparse Eject = "Eject"
unparse LogOff = "LogOff"
unparse Power = "Power"
unparse PowerOff = "PowerOff"
unparse PrintScreen = "PrintScreen"
unparse Hibernate = "Hibernate"
unparse Standby = "Standby"
unparse WakeUp = "WakeUp"
unparse AllCandidates = "AllCandidates"
unparse Alphanumeric = "Alphanumeric"
unparse CodeInput = "CodeInput"
unparse Compose = "Compose"
unparse Convert = "Convert"
unparse Dead = "Dead"
unparse FinalMode = "FinalMode"
unparse GroupFirst = "GroupFirst"
unparse GroupLast = "GroupLast"
unparse GroupNext = "GroupNext"
unparse GroupPrevious = "GroupPrevious"
unparse ModeChange = "ModeChange"
unparse NextCandidate = "NextCandidate"
unparse NonConvert = "NonConvert"
unparse PreviousCandidate = "PreviousCandidate"
unparse Process = "Process"
unparse SingleCandidate = "SingleCandidate"
unparse HangulMode = "HangulMode"
unparse HanjaMode = "HanjaMode"
unparse JunjaMode = "JunjaMode"
unparse Eisu = "Eisu"
unparse Hankaku = "Hankaku"
unparse Hiragana = "Hiragana"
unparse HiraganaKatakana = "HiraganaKatakana"
unparse KanaMode = "KanaMode"
unparse KanjiMode = "KanjiMode"
unparse Katakana = "Katakana"
unparse Romaji = "Romaji"
unparse Zenkaku = "Zenkaku"
unparse ZenkakuHankaku = "ZenkakuHankaku"
unparse (F i) = "F" <> show i
unparse (Soft i) = "Soft" <> show i
unparse ChannelDown = "ChannelDown"
unparse ChannelUp = "ChannelUp"
unparse Close = "Close"
unparse MailForward = "MailForward"
unparse MailReply = "MailReply"
unparse MailSend = "MailSend"
unparse MediaClose = "MediaClose"
unparse MediaFastForward = "MediaFastForward"
unparse MediaPause = "MediaPause"
unparse MediaPlay = "MediaPlay"
unparse MediaPlayPause = "MediaPlayPause"
unparse MediaRecord = "MediaRecord"
unparse MediaRewind = "MediaRewind"
unparse MediaStop = "MediaStop"
unparse MediaTrackNext = "MediaTrackNext"
unparse MediaTrackPrevious = "MediaTrackPrevious"
unparse New = "New"
unparse Open = "Open"
unparse Print = "Print"
unparse Save = "Save"
unparse SpellCheck = "SpellCheck"
unparse Key11 = "Key11"
unparse Key12 = "Key12"
unparse AudioBalanceLeft = "AudioBalanceLeft"
unparse AudioBalanceRight = "AudioBalanceRight"
unparse AudioBassBoostDown = "AudioBassBoostDown"
unparse AudioBassBoostToggle = "AudioBassBoostToggle"
unparse AudioBassBoostUp = "AudioBassBoostUp"
unparse AudioFaderFront = "AudioFaderFront"
unparse AudioFaderRear = "AudioFaderRear"
unparse AudioSurroundModeNext = "AudioSurroundModeNext"
unparse AudioTrebleDown = "AudioTrebleDown"
unparse AudioTrebleUp = "AudioTrebleUp"
unparse AudioVolumeDown = "AudioVolumeDown"
unparse AudioVolumeUp = "AudioVolumeUp"
unparse AudioVolumeMute = "AudioVolumeMute"
unparse MicrophoneToggle = "MicrophoneToggle"
unparse MicrophoneVolumeDown = "MicrophoneVolumeDown"
unparse MicrophoneVolumeUp = "MicrophoneVolumeUp"
unparse MicrophoneVolumeMute = "MicrophoneVolumeMute"
unparse SpeechCorrectionList = "SpeechCorrectionList"
unparse SpeechInputToggle = "SpeechInputToggle"
unparse (LaunchApplication i) = "LaunchApplication" <> show i
unparse LaunchCalendar = "LaunchCalendar"
unparse LaunchContacts = "LaunchContacts"
unparse LaunchMail = "LaunchMail"
unparse LaunchMediaPlayer = "LaunchMediaPlayer"
unparse LaunchMusicPlayer = "LaunchMusicPlayer"
unparse LaunchPhone = "LaunchPhone"
unparse LaunchScreenSaver = "LaunchScreenSaver"
unparse LaunchSpreadsheet = "LaunchSpreadsheet"
unparse LaunchWebBrowser = "LaunchWebBrowser"
unparse LaunchWebCam = "LaunchWebCam"
unparse LaunchWordProcessor = "LaunchWordProcessor"
unparse BrowserBack = "BrowserBack"
unparse BrowserFavorites = "BrowserFavorites"
unparse BrowserForward = "BrowserForward"
unparse BrowserHome = "BrowserHome"
unparse BrowserRefresh = "BrowserRefresh"
unparse BrowserSearch = "BrowserSearch"
unparse BrowserStop = "BrowserStop"
unparse AppSwitch = "AppSwitch"
unparse Call = "Call"
unparse Camera = "Camera"
unparse CameraFocus = "CameraFocus"
unparse EndCall = "EndCall"
unparse GoBack = "GoBack"
unparse GoHome = "GoHome"
unparse HeadsetHook = "HeadsetHook"
unparse LastNumberRedial = "LastNumberRedial"
unparse Notification = "Notification"
unparse MannerMode = "MannerMode"
unparse VoiceDial = "VoiceDial"
unparse TV = "TV"
unparse TV3DMode = "TV3DMode"
unparse TVAntennaCable = "TVAntennaCable"
unparse TVAudioDescription = "TVAudioDescription"
unparse TVAudioDescriptionMixDown = "TVAudioDescriptionMixDown"
unparse TVAudioDescriptionMixUp = "TVAudioDescriptionMixUp"
unparse TVContentsMenu = "TVContentsMenu"
unparse TVDataService = "TVDataService"
unparse TVInput = "TVInput"
unparse (TVInputComponent i) = "TVInputComponent" <> show i
unparse (TVInputComposite i) = "TVInputComposite" <> show i
unparse (TVInputHDMI i) = "TVInputHDMI" <> show i
unparse TVInputVGA1 = "TVInputVGA1"
unparse TVMediaContext = "TVMediaContext"
unparse TVNetwork = "TVNetwork"
unparse TVNumberEntry = "TVNumberEntry"
unparse TVPower = "TVPower"
unparse TVRadioService = "TVRadioService"
unparse TVSatellite = "TVSatellite"
unparse TVSatelliteBS = "TVSatelliteBS"
unparse TVSatelliteCS = "TVSatelliteCS"
unparse TVSatelliteToggle = "TVSatelliteToggle"
unparse TVTerrestrialAnalog = "TVTerrestrialAnalog"
unparse TVTerrestrialDigital = "TVTerrestrialDigital"
unparse TVTimer = "TVTimer"
unparse AVRInput = "AVRInput"
unparse AVRPower = "AVRPower"
unparse ColorF0Red = "ColorF0Red"
unparse ColorF1Green = "ColorF1Green"
unparse ColorF2Yellow = "ColorF2Yellow"
unparse ColorF3Blue = "ColorF3Blue"
unparse ColorF4Grey = "ColorF4Grey"
unparse ColorF5Brown = "ColorF5Brown"
unparse ClosedCaptionToggle = "ClosedCaptionToggle"
unparse Dimmer = "Dimmer"
unparse DisplaySwap = "DisplaySwap"
unparse DVR = "DVR"
unparse Exit = "Exit"
unparse (FavoriteClear i) = "FavoriteClear" <> show i
unparse (FavoriteRecall i) = "FavoriteRecall" <> show i
unparse (FavoriteStore i) = "FavoriteStore" <> show i
unparse Guide = "Guide"
unparse GuideNextDay = "GuideNextDay"
unparse GuidePreviousDay = "GuidePreviousDay"
unparse Info = "Info"
unparse InstantReplay = "InstantReplay"
unparse Link = "Link"
unparse ListProgram = "ListProgram"
unparse LiveContent = "LiveContent"
unparse Lock = "Lock"
unparse MediaApps = "MediaApps"
unparse MediaAudioTrack = "MediaAudioTrack"
unparse MediaLast = "MediaLast"
unparse MediaSkipBackward = "MediaSkipBackward"
unparse MediaSkipForward = "MediaSkipForward"
unparse MediaStepBackward = "MediaStepBackward"
unparse MediaStepForward = "MediaStepForward"
unparse MediaTopMenu = "MediaTopMenu"
unparse NavigateIn = "NavigateIn"
unparse NavigateNext = "NavigateNext"
unparse NavigateOut = "NavigateOut"
unparse NavigatePrevious = "NavigatePrevious"
unparse NextFavoriteChannel = "NextFavoriteChannel"
unparse NextUserProfile = "NextUserProfile"
unparse OnDemand = "OnDemand"
unparse Pairing = "Pairing"
unparse PinPDown = "PinPDown"
unparse PinPMove = "PinPMove"
unparse PinPToggle = "PinPToggle"
unparse PinPUp = "PinPUp"
unparse PlaySpeedDown = "PlaySpeedDown"
unparse PlaySpeedReset = "PlaySpeedReset"
unparse PlaySpeedUp = "PlaySpeedUp"
unparse RandomToggle = "RandomToggle"
unparse RcLowBattery = "RcLowBattery"
unparse RecordSpeedNext = "RecordSpeedNext"
unparse RfBypass = "RfBypass"
unparse ScanChannelsToggle = "ScanChannelsToggle"
unparse ScreenModeNext = "ScreenModeNext"
unparse Settings = "Settings"
unparse SplitScreenToggle = "SplitScreenToggle"
unparse STBInput = "STBInput"
unparse STBPower = "STBPower"
unparse Subtitle = "Subtitle"
unparse Teletext = "Teletext"
unparse VideoModeNext = "VideoModeNext"
unparse Wink = "Wink"
unparse ZoomToggle = "ZoomToggle"

fromModifier :: Modifier -> Key
fromModifier Modifier.Alt = Alt
fromModifier Modifier.AltGraph = AltGraph
fromModifier Modifier.CapsLock = CapsLock
fromModifier Modifier.Control = Control
fromModifier Modifier.Fn = Fn
fromModifier Modifier.FnLock = FnLock
fromModifier Modifier.Meta = Meta
fromModifier Modifier.NumLock = NumLock
fromModifier Modifier.ScrollLock = ScrollLock
fromModifier Modifier.Shift = Shift
fromModifier Modifier.Symbol = Symbol
fromModifier Modifier.SymbolLock = SymbolLock
fromModifier Modifier.Hyper = Hyper
fromModifier Modifier.Super = Super

toModifier :: Key -> Maybe Modifier
toModifier Alt = Just Modifier.Alt
toModifier AltGraph = Just Modifier.AltGraph
toModifier CapsLock = Just Modifier.CapsLock
toModifier Control = Just Modifier.Control
toModifier Fn = Just Modifier.Fn
toModifier FnLock = Just Modifier.FnLock
toModifier Meta = Just Modifier.Meta
toModifier NumLock = Just Modifier.NumLock
toModifier ScrollLock = Just Modifier.ScrollLock
toModifier Shift = Just Modifier.Shift
toModifier Symbol = Just Modifier.Symbol
toModifier SymbolLock = Just Modifier.SymbolLock
toModifier Hyper = Just Modifier.Hyper
toModifier Super = Just Modifier.Super
toModifier _ = Nothing