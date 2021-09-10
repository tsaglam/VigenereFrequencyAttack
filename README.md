# Vigenere Frequency Attack
A simple demonstration of a frequency attack on a [Vigenère cipher](https://en.wikipedia.org/wiki/Vigenère_cipher) implemented in Haskell.
This attack is based on the [Friedman test](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher#Friedman_test) and [frequency analysis](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher#Frequency_analysis).

Call `decodeVigenere code 5` to decode the cipher text `code` with a maximal key length of 5.

The cipher text `code` is:
```
GGAAKWRJTUWQHWSGAOAADBFTCGLYSFGWRCGLYASPGNSARZANTLHNLCBFTESRLLOJZAGOEGWNQLOGZIACPRGPYWNBLOADYPSRRLHRQCNJENDOGLHRHRBTLREIFOESWEYVIFWMCGWRJEQOESWEYDIXWWRUAALDBSNLLHVFGNTOHLIGKOJWMNQAFOEYDNBLTEQIGKGBANTLOOWAYGNTHRBUEFKBHLTUSTFLAELIAYTBUHNFGRLHRLEPZNVUAYUOZEUAATLSNQSSCWCVSLFZOHLOHLTBWVRJYHFDRJPNADNFDBNEEOOECEQKTHVEALOHLTUWRROOECIAYOALHVKYBMAEWTUWNBTLRSTYSSYAFGANTMPGZETDOOWIAGUEOIYVLLANRIUVLAODEPMREWNGKYFLEZASVFAYGTBXWNQLRXTUGLQANTLHRTATGNGZIFGNRTYIARGMEBXTUWNNLUEWOSLHRHRBTLRESOMTGZAGKNBLAYDBNVTJGTUGUFSNQLHVJTRWNSGRNDOGGFRFGVFERJSNFDEWSRSRPZEFOAFSKVFDBXAGGMVUMBEEALFBJCBEPHLEEKCVWNPWMHUHYAKRHHLKIPKPBKTZSNUSTGSNCJOWWCGSNRFTVJESAEYVOSJEFWAEUHGZAGOAFTRBSDYQACGLVLIPSLEWAYAZRVTUSTJGRXANGWNQWDGGIZHRBNEGZEUMMNFCBFDVLIBFCBMLQSLFGBRKUONEELEQLOQWGESDRATRVWNJDFFOJVEA
```
