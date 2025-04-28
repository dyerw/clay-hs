-- | Compares Haskell implementation to C implementaiton
-- to ensure they create the same render commands
module Equivalence.ClayWebsite where

import Clay.Color
import Clay.Layout
import Data.Text
import Test.Hspec

spec :: Spec
spec = do
  pure ()

-- Implement Clay website
data Fonts = BodyFont | TitleFont

type ImageUrl = Text

data Events = RendererButtonClicked

type Custom = ()

type WebsiteElement = Clay Events Fonts ImageUrl Custom

headerText :: Text -> WebsiteElement
headerText =
  text
    ( font BodyFont
        <> textColor (Color 61 26 5 255)
        <> fontSize 24
    )

landingPageBlob :: Int -> Color -> ImageUrl -> Text -> WebsiteElement
landingPageBlob fs color img t =
  element
    "HeroBlob"
    ( paddingAll 16
        <> grow (maxSize 480)
        <> childGap 16
        <> childAlignY AlignYCenter
        <> border 2 color
        -- <> cornerRadius 10
    )
    [ image "CheckImage" img (fixed 32),
      text (textColor color <> fontSize fs) t
    ]

orange :: Color
orange = Color 230 100 0 255

colorBlobBorder1 :: Color
colorBlobBorder1 = Color 230 100 0 255

colorBlobBorder2 :: Color
colorBlobBorder2 = Color 230 100 0 255

colorBlobBorder3 :: Color
colorBlobBorder3 = Color 230 100 0 255

colorBlobBorder4 :: Color
colorBlobBorder4 = Color 230 100 0 255

colorBlobBorder5 :: Color
colorBlobBorder5 = Color 230 100 0 255

landingPageDesktop :: WebsiteElement
landingPageDesktop =
  element
    "LandingPage1Desktop"
    (growX_ <> fitY (maxSize $ viewHeight - 70) <> fitX_ <> childAlignY AlignYCenter <> paddingAll 50)
    [ element
        "LandingPage1"
        (grow_ <> childAlignY AlignYCenter <> childGap 32 <> paddingAll 32 <> borderX 2 red)
        [ element
            "LeftText"
            (percentX 0.55 <> topToBottom <> childGap 8)
            [ text (fontSize 56 <> font TitleFont <> textColor red) "Clay is a blah blah blah",
              element "LandingPageSpacer" (grow_ <> fixed 32) [],
              text (fontSize 36 <> font TitleFont <> textColor orange) "Blah"
            ],
          element
            "HeroImageOuter"
            (topToBottom <> percentX 0.45 <> childAlignX AlignXCenter <> childGap 16)
            [ landingPageBlob 32 colorBlobBorder5 "High performance" "/clay/images/check_5.png",
              landingPageBlob 32 colorBlobBorder4 "Flexbox-style responsive layout" "check_6.png",
              landingPageBlob 32 colorBlobBorder3 "Flexbox-style responsive layout" "check_6.png",
              landingPageBlob 32 colorBlobBorder2 "Flexbox-style responsive layout" "check_6.png",
              landingPageBlob 32 colorBlobBorder1 "Flexbox-style responsive layout" "check_6.png"
            ]
        ]
    ]

--
-- button :: e -> Color -> Color -> Text -> Style e
-- button event bgColor bgHoverColor t =
--   el_
--     ( onClick event
--         <> fixed 300
--         <> paddingAll 16
--         <> border 2 red
--         <> backgroundColor bgColor
--         <> backgroundColorHovered bgHoverColor
--         <> cornerRadius 10
--     )
--     [ text
--         ( fontSize 26
--             <> font BodyFont
--             <> textColor colorLight
--         )
--         t
--     ]

-- rendererButtonActive :: Text -> HomepageElement
-- rendererButtonActive t =
--   button RendererButtonClicked colorRed colorRedHover t

-- rendererButtonInactive :: Text -> HomepageElement
-- rendererButtonInactive t =
--  button RendererButtonClicked colorLight colorLightHover t
