module Vigenere where

import Data.List
import Data.Tuple
import Data.Char

code = "GGAAKWRJTUWQHWSGAOAADBFTCGLYSFGWRCGLYASPGNSARZANTLHNLCBFTESRLLOJZAGOEGWNQLOGZIACPRGPYWNBLOADYPSRRLHRQCNJENDOGLHRHRBTLREIFOESWEYVIFWMCGWRJEQOESWEYDIXWWRUAALDBSNLLHVFGNTOHLIGKOJWMNQAFOEYDNBLTEQIGKGBANTLOOWAYGNTHRBUEFKBHLTUSTFLAELIAYTBUHNFGRLHRLEPZNVUAYUOZEUAATLSNQSSCWCVSLFZOHLOHLTBWVRJYHFDRJPNADNFDBNEEOOECEQKTHVEALOHLTUWRROOECIAYOALHVKYBMAEWTUWNBTLRSTYSSYAFGANTMPGZETDOOWIAGUEOIYVLLANRIUVLAODEPMREWNGKYFLEZASVFAYGTBXWNQLRXTUGLQANTLHRTATGNGZIFGNRTYIARGMEBXTUWNNLUEWOSLHRHRBTLRESOMTGZAGKNBLAYDBNVTJGTUGUFSNQLHVJTRWNSGRNDOGGFRFGVFERJSNFDEWSRSRPZEFOAFSKVFDBXAGGMVUMBEEALFBJCBEPHLEEKCVWNPWMHUHYAKRHHLKIPKPBKTZSNUSTGSNCJOWWCGSNRFTVJESAEYVOSJEFWAEUHGZAGOAFTRBSDYQACGLVLIPSLEWAYAZRVTUSTJGRXANGWNQWDGGIZHRBNEGZEUMMNFCBFDVLIBFCBMLQSLFGBRKUONEELEQLOQWGESDRATRVWNJDFFOJVEA"

engl = [('A', 0.08167),('B', 0.01492), ('C', 0.02782), ('D', 0.04253), ('E', 0.12702), ('F', 0.02228), ('G', 0.02015), ('H', 0.06094), ('I', 0.06966), ('J', 0.00153), ('K', 0.00772),	('L', 0.04025),	('M', 0.02406),	('N', 0.06749),	('O', 0.07507),	('P', 0.01929),('Q', 0.095),	('R', 0.05987),	('S', 6.327),	('T', 0.09056),	('U', 0.02758),	('V', 0.00978),	('W', 0.02361),	('X', 0.00150),	('Y', 0.01974),	('Z', 0.00074)]
englPlain = snd (unzip engl)
abc = fst (unzip engl)

{- MAIN METHOD (call decodeVigenere code 5) -}
decodeVigenere cypher maxKeyLength = decode cypher (createKey cypher maxKeyLength)

{- DECODER: -}

{- decodes vigenere cypher with a given key -}
decode cypher key = decodeLoop cypher 0
	where
		decodeLoop [] _ = []
		decodeLoop (cy:pher) n = decodeChar cy (key !! (n `mod` (length key))) : decodeLoop pher (n+1) 

{- adds two chars to a new char e.g. cypherchar + key = decypher -}
decodeChar char key = let [c, k] = map (\c -> (ord c) - (ord 'A')) [char, key] in chr (((c - k) `mod` 26) + (ord 'A'))

{- takes a cypher and and a maximal key length. calculates key length and and creates a key from letter distributions -}
createKey cypher maxKeyLength = map (findLanguageShift.createLetterDistribution) (makeFragmentation cypher (keyLength cypher maxKeyLength))

{- finds with shift from A to Z of the letter distribution is closest to the english alphabet-}
findLanguageShift letterDist = abc !! findIndexOfMin ([correlateLanguage englPlain (shift letterDist x) | x <- [0..25]])

{- how similar are two given letter distributions -}
correlateLanguage a b = foldr (+) 0 (map abs (zipWith (-) a b))

{- creates chances for letters in a string -}
createLetterDistribution string = map prob (group (sort (string ++ abc)))
	where
		prob list = fromIntegral (length list - 1) / fromIntegral (length (string))

{- creates keyLength lists and fills for 1 to n the n-th list with every n-th char -}
makeFragmentation cypher keyLength = reverse $ fragmentation cypher keyLength
	where
		fragmentation _ 0 = []
		fragmentation cypher2 ctr = takeEvery keyLength cypher2 : fragmentation ((head cypher):cypher2) (ctr-1)

{- KEYLENGTH FINDER: -}

{- returns the probabilities for different keylengths from 1 to maxLength -}
keyLengthChances cypher 0 = []
keyLengthChances cypher maxLength = correlate cypher (shift cypher maxLength) : keyLengthChances cypher (maxLength - 1)

{- returns the top guess for the keylength of a cypher -}
keyLength cypher maxLength = 1 + findIndexOfMin (keyLengthChances cypher maxLength)

{- LIBRARY FUNCTIONS: -}

{- takes every n-th element from list -}
takeEvery _ [] = []
takeEvery n list | length list < n = []
				 | otherwise = let (left, right) = splitAt n list in last left : takeEvery n right

{- shift string n chars to the textleft -}
shift string 0 = string 
shift (s:tring) n = shift (tring ++ [s]) (n - 1)

{- returns a value of correlation for two strings/arrays -}
correlate [] [] = 0
correlate (a:ab) (x:xy) = abs ((ord a) - (ord x)) + correlate ab xy

{- returns the index of the minimum of the list -}
findIndexOfMin list = val (elemIndex (minimum list) list)

{- makes Int from Maybe Int with Nothing = -1 -}
val (Just x) = x
val Nothing = -1
		
{- rounds float -}
roundFloat n  f= (fromInteger $ round $ f * (10^n)) / (10.0^^n)