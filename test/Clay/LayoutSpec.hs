module Clay.LayoutSpec where

import Hspec.Test
import Clay.Layout

spec :: Spec
spec = do
  pure ()

-- Implement Clay website
data Fonts = BodyFont | TitleFont

type ImageUrl = Text

data Events = RendererButtonClicked

type HomepageElement = Element Events Fonts ImageUrl

headerText = TextElement
  (TextElementConfig (61, 26, 5, 255) BodyFont 24)

landingPageBlob :: Int -> Int -> Color -> ImageUrl -> String
landingPageBlob idx fs c text imageUrl =
  elIx
    "HeroBlob"
    idx
    (paddingAll 16 <> growMax 480 <>
     childGap 16 <> childAlign YCenter <>
     (border 2 c) <> cornerRadius 10)
    [
      imageIx "CheckImage" idx imageUrl (fixed 32),
      text (color c <> fontSize fs) 
    ]

landingPageDesktop :: HomepageElement
landingPageDesktop =
  el "LandingPage1Desktop"
    (growX <> (fitMin $ WH - 70) <> childAlign YCenter <> paddingAll 50)
    [
      el "LandingPage1"
        (grow <> childAlign YCenter <> childGap 32 <> paddingAll 32 <> borderRL 2 red)
        [ el "LeftText"
            (percentX 0.55 <> topToBottom <> childGap 8)
            [ text (fontSize 56 <> TitleFont <> red) "Clay is a blah blah blah"
            , el "LandingPageSpacer" (grow <> fixed 32)
            , text (fontSize 36 <> TitleFont <> orange)
            ]
        , el "HeroImageOuter"
            (topToBottom <> percentX 0.45 <> childAlign XCenter <> childGap 16)
            [ landingPageBlob 1 32 colorBlobBorder5 "High performance" "/clay/images/check_5.png"
            , landingPageBlob 2 32 colorBlobBorder4 "Flexbox-style responsive layout" "check_6.png"
            , landingPageBlob 3 32 colorBlobBorder3 "Flexbox-style responsive layout" "check_6.png"
            , landingPageBlob 4 32 colorBlobBorder2 "Flexbox-style responsive layout" "check_6.png"
            , landingPageBlob 5 32 colorBlobBorder1 "Flexbox-style responsive layout" "check_6.png"
            ]
        ]
    ]

button :: e -> Color -> Color -> Text -> Style e f
button event bgColor bgHoverColor t =
  el_
    (onClick event
     <> fixed 300
     <> paddingAll 16
     <> border 2 red
     <> backgroundColor bgColor
     <> backgroundColorHovered bgHoverColor
     <> cornerRadius 10)
    [text
      (fontSize 26
      <> font BodyFont
      <> textColor colorLight)
      t
    ]

rendererButtonActive :: Text -> HomepageElement
rendererButtonActive t =
  button RendererButtonClicked colorRed colorRedHover t

rendererButtonInactive :: Text -> HomepageElement
rendererButtonInactive t =
  button RendererButtonClicked colorLight colorLightHover t
