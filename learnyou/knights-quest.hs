import Data.Monoid
import Control.Applicative

-- 8x8 board, knight can move to one of 8 positions if they're on
-- the board. Can he move to a give position in n moves?
--
-- takes initial position, returns a list of paths

knight_moves_all (x, y) = [
   (x + 2, y - 1),
   (x + 2, y + 1),
   (x - 2, y - 1),
   (x - 2, y + 1),
   (x + 1, y - 2),
   (x + 1, y + 2),
   (x - 1, y - 2),
   (x - 1, y + 2)]

knight_moves pos = 
   filter (\(x, y) -> x >= 0 && 
                      x <= 8 && 
                      y >= 0 && 
                      y <= 8) $ knight_moves_all pos

three_moves init = [init] >>= knight_moves >>= knight_moves >>= knight_moves


--paths init = mappend init paths (knight_moves init)
--knight_paths curr =

--knight_moves :: (Int, Int) -> [(Int, Int)]
--knight_moves (currx, curry) = [
