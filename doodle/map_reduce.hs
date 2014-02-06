
import qualified Data.Map.Lazy as Map

type MapFun rec key val = rec -> [(key, [val])]
type ReduceFun key val result = (key, [val]) -> [result]

map_reduce :: Ord key => (MapFun rec key val) -> (ReduceFun key val result) -> [rec] -> [result]
map_reduce map_fun reduce_fun rs =
   concatMap reduce_fun (Map.toList mapped)
   where
      map_results = concatMap map_fun rs
      insert_result m (k, v) = Map.insertWith (\o n -> o ++ n) k v m
      mapped = foldl insert_result Map.empty map_results
      
test_rec = ["there", "is", "a", "good", "sized", "pile",
            "of", "green", "apples", "and", "pink", "apples",
            "and", "good", "red", "apples", "over", "there"]

-- word histogram
my_map :: String -> [(String, [Int])]
my_map s = [(s, [1])]

my_reduce :: (String, [Int]) -> [(String, Int)]
my_reduce (s, occs) = [(s, (sum occs))]

word_counts = map_reduce my_map my_reduce test_rec

-- count a particular character
count_letter_in_word letter word = [(1, [length $ filter (\c -> c == letter) word])]
add_letter_counts (_, counts) = [sum counts]

letter_counts c = map_reduce (count_letter_in_word c) add_letter_counts test_rec

