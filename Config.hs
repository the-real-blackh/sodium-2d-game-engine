{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses,
        StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Config where

import Platform
import Resources

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Data.Text (Text)


instance XmlPickler [UNode Text] ([Int], Maybe Int) where
    xpickle =
        xpPair
            (xpElemNodes "theses" $ xpList0 $ xpElemAttrs "these" $ xpAttr "number" xpPrim)
            (xpOption $ xpElemAttrs "by" $ xpAttr "number" xpPrim)

data BackgroundRef p = StockBackground Key
                     | AssetBackground (AssetRef p)

deriving instance Eq (AssetRef p) => Eq (BackgroundRef p)
deriving instance Show (AssetRef p) => Show (BackgroundRef p)

instance Platform p => XmlPickler [UNode Text] (BackgroundRef p) where
    xpickle =
        xpAlt
            (\ref -> case ref of
                 StockBackground _ -> 0
                 AssetBackground _ -> 1)
            [
                xpWrap (StockBackground, \(StockBackground k) -> k) $
                xpElemAttrs "stock" (xpAttr "key" xpickle),
                xpWrap (AssetBackground, \(AssetBackground ref) -> ref) $
                xpElemNodes "asset" xpickle
            ]

data Config p = Config {
        coSoundsOn      :: Bool,
        coComplimentsOn :: Bool,
        coBackground    :: BackgroundRef p,
        coSelected      :: ([Int], Maybe Int),
        coYourPhotos    :: Bool
    }

emptyConfig :: Platform p => Resources p -> Config p
emptyConfig res = Config {
        coSoundsOn      = True,
        coComplimentsOn = True,
        coBackground    = StockBackground (keyOf $ backgrounds_largeYellowGreen2048x1536 res),
        coSelected      = ([], Nothing),
        coYourPhotos    = False
    }

instance Platform p => XmlPickler [UNode Text] (Config p) where
    xpickle =
        xpWrap (\((so, co, yo), (bg, se)) -> Config so co bg se yo,
                \(Config so co bg se yo) -> ((so, co, yo), (bg, se))) $
        xpElem "tables-settings"
            (xpTriple
                (xpAttr "soundsOn" xpPrim)
                (xpAttr "complimentsOn" xpPrim)
                (xpAttr "yourPhotos" xpPrim))
            (xpPair xpickle xpickle)

