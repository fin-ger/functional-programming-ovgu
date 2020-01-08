-- *Main> "" == []
-- True
-- A string is an array of characters, so an empty string is also an empty array

-- *Main> tail [1] == ""
--
-- <interactive>:50:7: error:
--     • No instance for (Num Char) arising from the literal ‘1’
--     • In the expression: 1
--       In the first argument of ‘tail’, namely ‘[1]’
--       In the first argument of ‘(==)’, namely ‘tail [1]’
-- 1 is not a Char as required for string

-- *Main> tail [1] == []
-- True
-- empty array equals empty array. Glad this is true... It saves lives!

-- *Main> read "10" == 10
-- True
-- Great parsing of ints from string

-- *Main> read "10"
-- *** Exception: Prelude.read: no parse
-- ghci does not know to which type the string should be parsed to

-- *Main> let x = read "10" :: Int
-- *Main> x
-- 10
-- Now ghci likes our types :D
