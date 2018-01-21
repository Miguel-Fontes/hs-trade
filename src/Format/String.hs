module Format.String (
    lpad,
    rpad
) where

lpad :: Int -> Char -> String -> String
lpad = pad (\filler str -> filler ++ str)

rpad :: Int -> Char -> String -> String
rpad = pad (\filler str -> str ++ filler)
                      
pad :: (String -> String -> String) -> Int -> Char -> String -> String
pad f size char str = let t = size - (length str) 
                       in f (take t (repeat char)) str