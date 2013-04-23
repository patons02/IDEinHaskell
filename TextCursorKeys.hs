{--------------------------------------------------------------}
{- CS410 Advanced Functional Programming                      -}
{- Practical 1: A Text Editor                                 -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- SUBMIT YOUR VERSION OF THIS FILE BY EMAIL TO CONOR         -}
{- DEADLINE: midnight, Thursday 11 October                    -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- IDENTIFY YOURSELF:                                         -}
{- Name: Stuart Paton     		                      -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- THIS CODE WILL COMPILE UNDER UNIX BUT NOT WINDOWS          -}
{-                                                            -}
{- To compile the project, use shell command                  -}
{-   make                                                     -}
{- To run your editor, try                                    -}
{-   ./credit <filename>                                      -}
{- or                                                         -}
{-   ./credit                                                 -}
{- to start from blank.                                       -}
{- To quit the editor, press ESCAPE.                          -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- This practical makes use of a bunch of other files I've    -}
{- written, including the layout file from last time. This is -}
{- the only file you should modify. Don't rename this file!   -} 
{--------------------------------------------------------------}

module TextCursorKeys where

import Block
import Overlay

{--------------------------------------------------------------}
{- This module starts with some equipment I've provided for   -}
{- you. To get going, you should not need to make any changes -}
{- but to try more sophisticated things, e.g. editing with    -}
{- selection, cut, and paste, you may want to make changes.   -}
{-                                                            -}
{- Your main mission is to implement handleKey, down below.   -}
{--------------------------------------------------------------}

{- From the lecture, here's that type of backward lists. -}

data Bwd x = B0 | Bwd x :< x deriving (Show, Eq)

{- A slight improvement on the lecture: this is a cursor with x things
   round the outside, and an m in the middle. The idea is that we keep
   everything in exactly the right order, so you can always see what's
   where. -}

type Cursor x m = (Bwd x, m, [x])

{- Here's something to put in the middle, to show where you are. -}

data Here = Here deriving Show

{- If you start working with selections, you may wish to modify the
   Here type to account for the current state, e.g. {no selection,
   selection left of cursor, or selection right of cursor}.      -}

{- For one line, we have characters either side, and Here in the middle. -}

type StringCursor = Cursor Char Here 
    
{- For multiple lines, we have strings above and below, and a StringCursor
   in the middle. -}

type TextCursor = Cursor String StringCursor 

{- Useful equipment: deactivate turns a cursor into a list by shuffling
   everything to the right, but it also tells you /numerically/ where the
   cursor was. This might help you implement up and down, for example. -}

deactivate :: Cursor x Here -> (Int, [x])
deactivate c = outward 0 c where
  outward i (B0, Here, xs)       = (i, xs)
  outward i (xz :< x, Here, xs)  = outward (i + 1) (xz, Here, x : xs)

{- Activate turns a list into a cursor open at the given position, or as
   near as it gets. -}

activate :: (Int, [x]) -> Cursor x Here
activate (i, xs) = inward i (B0, Here, xs) where
  inward _ c@(_, Here, [])     = c  -- we can go no further
  inward 0 c                   = c  -- we should go no further
  inward i (xz, Here, x : xs)  = inward (i - 1) (xz :< x, Here, xs)  -- and on!

{- Now, if you give me a TextCursor, I can compute the corresponding
   Layout Box, together with the coordinates of Here.
   This is how my code figures out what to display and where to put the
   cursor. -}

whatAndWhere :: TextCursor -> (Layout Box, Point)
whatAndWhere (czz, cur, css) = (foldr (joinV . layS) layZ strs, (x, y)) where
  (x, cs) = deactivate cur
  (y, strs) = deactivate (czz, Here, cs : css)

{- Next, you'll need some model of keystrokes. Here's a type describing
   some keystrokes. You may want more. -}

data ArrowDir = UpArrow | DownArrow | LeftArrow | RightArrow
data Modifier = Normal | Shift | Control

data Key
  = CharKey Char                -- an ordinary printable character
  | ArrowKey Modifier ArrowDir  -- an arrow key
  | Return
  | Backspace
  | Delete
  | Save
  | Load
  | Quit

{- Keys come in as standard ANSI escape sequences. You can look 'em up
   online. Feel free to extend escapeKeys so that more keystrokes get
   translated. -}

directions :: [(Char, ArrowDir)]
directions = [('A', UpArrow), ('B', DownArrow),
              ('C', RightArrow), ('D', LeftArrow)]

escapeKeys :: [(String, Key)]
escapeKeys =
  [([c], ArrowKey Normal d) | (c, d) <- directions] ++
  [("1;2" ++ [c], ArrowKey Shift d) | (c, d) <- directions] ++
  [("1;5" ++ [c], ArrowKey Control d) | (c, d) <- directions] ++
  [("3~", Delete)]
  

{- Last but not least, you get to tell my code how much damage you've done.
   This makes the redrawing more efficient: if you've done less damage to
   the file, my code needs to do less to update. If in doubt, overestimate
   the damage: a slow display is better than a broken display. -}

data Damage
  = NoChange       -- use this if nothing at all happened
  | PointChanged   -- use this if you moved the cursor but kept the text
  | LineChanged    -- use this if you changed text only on the current line
  | LotsChanged    -- use this if you changed text off the current line
  deriving (Show, Eq, Ord)

{--------------------------------------------------------------------------}
{- AT LAST, YOUR BIT!                                                     -}
{- Given a Key and an initial TextCursor, either reject the keystroke or  -}
{- return a modified cursor, with an overestimate of the damage you've    -}
{- you've done. To give you the idea, I've supplied a broken version of   -}
{- ordinary typing, which you get to fix.                                 -}
{-                                                                        -}
{- Be creative!                                                           -}
{--------------------------------------------------------------------------}

handleKey :: Key -> TextCursor -> Maybe (Damage, TextCursor)
handleKey (CharKey c) (sz, (cz, Here, cs), ss)
               = Just (LineChanged, (sz, (cz :< c, Here, cs), ss))

--arrow key (left, right) over one line
handleKey (ArrowKey Normal RightArrow) (sz, (cz, Here, (c:cs)), ss)
	       = Just (PointChanged, (sz, (cz :< c, Here, cs), ss))

handleKey(ArrowKey Normal LeftArrow) (sz, ((cz :< c), Here, cs), ss) 
	       = Just (PointChanged, (sz, (cz, Here, (c:cs)), ss))

handleKey(ArrowKey Normal UpArrow) (sz :< s, (cz, Here, cs), ss)
	       = Just (PointChanged, (sz, newLineBuf, oldLineBuf:ss))
                 where
                   (x, oldLineBuf) = deactivate(cz, Here, cs)
                   newLineBuf = activate(x, cs)
                   
handleKey(ArrowKey Normal DownArrow) (sz, (cz, Here, cs), ss)
	       = Just (PointChanged, (sz :< oldLineBuf, newLineBuf, ss))
                 where
                   (x, oldLineBuf) = deactivate(cz, Here, cs)
                   newLineBuf = activate(x, cs)

-- enter key (eg create line break)
-- take the sz list and snoc the reverse of cz to it by converting cz into a forward list
-- This splits it into two lists and moves the cursor to the start of the new list.
handleKey Return (sz, (cz, Here, cs), ss)
	       = Just (LotsChanged, (sz:<(bwd2fwd cz), (B0, Here, cs), ss))

--backspace for one line
handleKey Backspace (sz, ((cz :< c), Here, cs), ss)
	       = Just (LineChanged, (sz, (cz, Here, cs), ss))
handleKey Backspace (sz, ((cz :< c), Here, cs), ss)
               = Just (LotsChanged, (sz :< (bwd2fwd cz), (cz, Here, cs), ss)) 
                 
--deletes from one line
handleKey Delete (sz, (cz, Here, (c:cs)), ss) 
	       = Just (LineChanged, (sz, (cz, Here, cs), ss))
handleKey Delete (sz, (cz, Here, (c:cs)), ss) 
               = Just (LotsChanged, (sz, (cz, Here, cs), ss))

handleKey _ _ = Nothing

bwd2fwd :: Bwd x -> [x]
bwd2fwd xz = snd (deactivate (xz, Here, []))

fwd2bwd :: [x] -> Bwd x
fwd2bwd xs = xz 
	where
	(xz, _, _) = activate ((length xs),xs)

