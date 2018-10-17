{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Copyright (C) 2001-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This file reflects the Gtk+ object hierarchy in terms of Haskell classes.
--
-- Note: the mk... functions were originally meant to simply be an alias
-- for the constructor. However, in order to communicate the destructor
-- of an object to objectNew, the mk... functions are now a tuple containing
-- Haskell constructor and the destructor function pointer. This hack avoids
-- changing all modules that simply pass mk... to objectNew.
--
module Graphics.Rendering.Pango.Types (

  module System.Glib.GObject,
  PangoContext(PangoContext), PangoContextClass,
  toPangoContext, 
  mkPangoContext, unPangoContext,
  castToPangoContext, gTypePangoContext,
  PangoLayoutRaw(PangoLayoutRaw), PangoLayoutRawClass,
  toPangoLayoutRaw, 
  mkPangoLayoutRaw, unPangoLayoutRaw,
  castToPangoLayoutRaw, gTypePangoLayoutRaw,
  Font(Font), FontClass,
  toFont, 
  mkFont, unFont,
  castToFont, gTypeFont,
  FontFamily(FontFamily), FontFamilyClass,
  toFontFamily, 
  mkFontFamily, unFontFamily,
  castToFontFamily, gTypeFontFamily,
  FontFace(FontFace), FontFaceClass,
  toFontFace, 
  mkFontFace, unFontFace,
  castToFontFace, gTypeFontFace,
  FontMap(FontMap), FontMapClass,
  toFontMap, 
  mkFontMap, unFontMap,
  castToFontMap, gTypeFontMap,
  FontSet(FontSet), FontSetClass,
  toFontSet, 
  mkFontSet, unFontSet,
  castToFontSet, gTypeFontSet
  ) where

import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
-- TODO work around cpphs https://ghc.haskell.org/trac/ghc/ticket/13553
#if __GLASGOW_HASKELL__ >= 707 || __GLASGOW_HASKELL__ == 0
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr (unsafeForeignPtrToPtr)
#endif

import Foreign.C.Types    (CULong(..), CUInt(..), CULLong(..))
import System.Glib.GType  (GType, typeInstanceIsA)
{#import System.Glib.GObject#}

{# context lib="gtk" prefix="gtk" #}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


-- *************************************************************** PangoContext

{#pointer *PangoContext as PangoContext foreign newtype #} deriving (Eq,Ord)

mkPangoContext = (PangoContext, objectUnref)
unPangoContext (PangoContext o) = o

class GObjectClass o => PangoContextClass o
toPangoContext :: PangoContextClass o => o -> PangoContext
toPangoContext = unsafeCastGObject . toGObject

instance PangoContextClass PangoContext
instance GObjectClass PangoContext where
  toGObject = GObject . castForeignPtr . unPangoContext
  unsafeCastGObject = PangoContext . castForeignPtr . unGObject

castToPangoContext :: GObjectClass obj => obj -> PangoContext
castToPangoContext = castTo gTypePangoContext "PangoContext"

gTypePangoContext :: GType
gTypePangoContext =
  {# call fun unsafe pango_context_get_type #}

-- ************************************************************* PangoLayoutRaw

{#pointer *PangoLayout as PangoLayoutRaw foreign newtype #} deriving (Eq,Ord)

mkPangoLayoutRaw = (PangoLayoutRaw, objectUnref)
unPangoLayoutRaw (PangoLayoutRaw o) = o

class GObjectClass o => PangoLayoutRawClass o
toPangoLayoutRaw :: PangoLayoutRawClass o => o -> PangoLayoutRaw
toPangoLayoutRaw = unsafeCastGObject . toGObject

instance PangoLayoutRawClass PangoLayoutRaw
instance GObjectClass PangoLayoutRaw where
  toGObject = GObject . castForeignPtr . unPangoLayoutRaw
  unsafeCastGObject = PangoLayoutRaw . castForeignPtr . unGObject

castToPangoLayoutRaw :: GObjectClass obj => obj -> PangoLayoutRaw
castToPangoLayoutRaw = castTo gTypePangoLayoutRaw "PangoLayoutRaw"

gTypePangoLayoutRaw :: GType
gTypePangoLayoutRaw =
  {# call fun unsafe pango_layout_get_type #}

-- *********************************************************************** Font

{#pointer *PangoFont as Font foreign newtype #} deriving (Eq,Ord)

mkFont = (Font, objectUnref)
unFont (Font o) = o

class GObjectClass o => FontClass o
toFont :: FontClass o => o -> Font
toFont = unsafeCastGObject . toGObject

instance FontClass Font
instance GObjectClass Font where
  toGObject = GObject . castForeignPtr . unFont
  unsafeCastGObject = Font . castForeignPtr . unGObject

castToFont :: GObjectClass obj => obj -> Font
castToFont = castTo gTypeFont "Font"

gTypeFont :: GType
gTypeFont =
  {# call fun unsafe pango_font_get_type #}

-- ***************************************************************** FontFamily

{#pointer *PangoFontFamily as FontFamily foreign newtype #} deriving (Eq,Ord)

mkFontFamily = (FontFamily, objectUnref)
unFontFamily (FontFamily o) = o

class GObjectClass o => FontFamilyClass o
toFontFamily :: FontFamilyClass o => o -> FontFamily
toFontFamily = unsafeCastGObject . toGObject

instance FontFamilyClass FontFamily
instance GObjectClass FontFamily where
  toGObject = GObject . castForeignPtr . unFontFamily
  unsafeCastGObject = FontFamily . castForeignPtr . unGObject

castToFontFamily :: GObjectClass obj => obj -> FontFamily
castToFontFamily = castTo gTypeFontFamily "FontFamily"

gTypeFontFamily :: GType
gTypeFontFamily =
  {# call fun unsafe pango_font_family_get_type #}

-- ******************************************************************* FontFace

{#pointer *PangoFontFace as FontFace foreign newtype #} deriving (Eq,Ord)

mkFontFace = (FontFace, objectUnref)
unFontFace (FontFace o) = o

class GObjectClass o => FontFaceClass o
toFontFace :: FontFaceClass o => o -> FontFace
toFontFace = unsafeCastGObject . toGObject

instance FontFaceClass FontFace
instance GObjectClass FontFace where
  toGObject = GObject . castForeignPtr . unFontFace
  unsafeCastGObject = FontFace . castForeignPtr . unGObject

castToFontFace :: GObjectClass obj => obj -> FontFace
castToFontFace = castTo gTypeFontFace "FontFace"

gTypeFontFace :: GType
gTypeFontFace =
  {# call fun unsafe pango_font_face_get_type #}

-- ******************************************************************** FontMap

{#pointer *PangoFontMap as FontMap foreign newtype #} deriving (Eq,Ord)

mkFontMap = (FontMap, objectUnref)
unFontMap (FontMap o) = o

class GObjectClass o => FontMapClass o
toFontMap :: FontMapClass o => o -> FontMap
toFontMap = unsafeCastGObject . toGObject

instance FontMapClass FontMap
instance GObjectClass FontMap where
  toGObject = GObject . castForeignPtr . unFontMap
  unsafeCastGObject = FontMap . castForeignPtr . unGObject

castToFontMap :: GObjectClass obj => obj -> FontMap
castToFontMap = castTo gTypeFontMap "FontMap"

gTypeFontMap :: GType
gTypeFontMap =
  {# call fun unsafe pango_font_face_get_type #}

-- ******************************************************************** FontSet

{#pointer *PangoFontset as FontSet foreign newtype #} deriving (Eq,Ord)

mkFontSet = (FontSet, objectUnref)
unFontSet (FontSet o) = o

class GObjectClass o => FontSetClass o
toFontSet :: FontSetClass o => o -> FontSet
toFontSet = unsafeCastGObject . toGObject

instance FontSetClass FontSet
instance GObjectClass FontSet where
  toGObject = GObject . castForeignPtr . unFontSet
  unsafeCastGObject = FontSet . castForeignPtr . unGObject

castToFontSet :: GObjectClass obj => obj -> FontSet
castToFontSet = castTo gTypeFontSet "FontSet"

gTypeFontSet :: GType
gTypeFontSet =
  {# call fun unsafe pango_fontset_get_type #}

