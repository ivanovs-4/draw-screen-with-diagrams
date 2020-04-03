#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (h: with h; [diagrams filepath])"

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Image
import System.FilePath


img :: Double -> String -> IO (Diagram B)
img size name = do
   res <- loadImageEmb $ "resource" </> name
   either (fail . show) (\img' -> pure $ image img' # sized (dims2D size size)) res


main :: IO ()
main = do
  barcodeImg    <- img 12 "barcode.png"
  photoImg      <- img iconSize "photo.png"
  exclamImg     <- img iconSize "exclam.png"
  rubImg        <- img iconSize "rub.png"
  telegramImg   <- img iconSize "telegram.png"
  let
    example :: Diagram B
    example = 
      (
        (
            (
              (
                barcodeImg
                ===
                strutY 4
                ===
                info # center # translateX (-9)
              ) # translateY 14

              <> (
                  hsep iconSep
                      [ square iconSize <> photoImg
                      , square iconSize <> exclamImg
                      , square iconSize <> rubImg
                      ] # alignBL # translateX (iconSep - (w/2)) # translateY (iconSep - (h/2))
                )

              <> (
                  hsep iconSep
                      [ square iconSize <> telegramImg
                      ] # alignBR # translateX ((-iconSep) + (w/2)) # translateY (iconSep - (h/2))
                )

              <>
              (rect w h # fc white)
            )
          <> (roundedRect (w+fh) (h+fh) r # lwG 0.4)
        )
        ===
        (strutY 3)
        ===
        (ta 0.7 "Отзыв" # bold)
      ) # lwG 0.2
      <>
      (arrowBetween ((-(w+3*fh)/2) ^& 0) ((-(w+1*fh)/2) ^& 0))
      <>
      (rect (w+3*fh) (h+4*fh))
  mainWith example

  where
    w = 38
    h = 62
    fh = 5
    r = 3
    iconSize = 5
    iconSep  = 0.8

    d1 =/= d2 = d1 === strutY 2 === d2

    t    t' = baselineText t'      # fontSize (local 2)
    ta x t' = (alignedText x 0.5) t' # fontSize (local 2.4)

    info = vsep (2 * 1.3) $
      [ t "Товар отсутствует:" # bold
      , t "Крупа гречневая,"
      , t "мешок 20кг."
      ]
