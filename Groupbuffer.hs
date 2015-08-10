module Groupbuffer where

import Graphics.UI.Gtk

data GroupBuffer = GBuffer{
  groupBufferWindow :: Window,
  content :: TextView,
  exitBuffer :: Button,
  wordWrap :: CheckButton
}
windowX = 600
windowY = 500

groupBufferLoad :: Builder -> IO GroupBuffer
groupBufferLoad builder = do
  groupBufferWindow <- builderGetObject builder castToWindow "groupbuffer"
  content <- builderGetObject builder castToTextView "content"
  exitBuffer <- builderGetObject builder castToButton "exitbuffer"
  wordWrap <- builderGetObject builder castToCheckButton "charwrap"
  let buffer = GBuffer groupBufferWindow content exitBuffer wordWrap
  groupBufferInitialize buffer
  return buffer

groupBufferInitialize :: GroupBuffer -> IO ()
groupBufferInitialize buffer = do
  let window = groupBufferWindow buffer
  onClicked (exitBuffer buffer) (widgetHide window)
  onClicked (wordWrap buffer) (changeWrap buffer)
  windowSetDefaultSize window windowX windowY
  windowSetDeletable window False

changeWrap :: GroupBuffer -> IO ()
changeWrap buffer = do
  active <- toggleButtonGetActive (wordWrap buffer)
  let textview = content buffer
  if active
    then textViewSetWrapMode textview WrapChar
    else textViewSetWrapMode textview WrapNone

runGroupBuffer :: GroupBuffer -> String -> IO ()
runGroupBuffer buffer fileContents = do
  textBuffer <- textViewGetBuffer $ content buffer
  textBufferSetText textBuffer fileContents
  widgetShowAll (groupBufferWindow buffer)
