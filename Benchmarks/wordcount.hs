import Foreign.C.Types
import System.IO
import System.IO.Unsafe

main = print . wordCount =<< getContents

wordCount x
 | head x == ' ' = 1 + (wordCount (tail x))
 | length x == 1 = 1
 | otherwise = wordCount (tail x)
