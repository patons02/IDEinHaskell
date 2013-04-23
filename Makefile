default: ide

ide: Main.hs Internal.hs TextEditor.hs TextCursorKeys.hs ANSIEscapes.hs Block.hs Overlay.hs
	ghc -lncurses --make Main -o textEditor

