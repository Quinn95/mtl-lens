
import Text.Parsec hiding ((<|>), many)
import Control.Applicative hiding (optional)
import Control.Monad


type Object = String
type Item = String

data Direction = North
               | South
               | East
               | West
               | Up
               | Down
               deriving (Eq, Show)

data Action = Move Direction -- go [north|south|east|west]
            | Look (Maybe String)
            | Attack Object (Maybe String) -- attack x [with y]
            | Use Item
            deriving (Show)
  

type Parser = ParsecT [Char] () IO

north, south, east, west :: Parser Direction 
north = abbrev "north" *> return North
south = abbrev "south" *> return South
east = abbrev "east"   *> return East
west = abbrev "west"   *> return West
up = abbrev "up"       *> return Up
down = abbrev "down"   *> return Down

direction = try north <|> try south <|> try east <|> try west <|> try up <|> down

abbrev :: String -> Parser Char 
abbrev (a:rest) = char a <* optional (string rest)

object :: [String] -> Parser String
object o  = choice $ string <$> o

weapon :: [String] -> Parser (Maybe String)
weapon w = optionMaybe $ spaces >> string "with" >> spaces >> object w

attack = do
    try $ abbrev "attack"
    spaces
    target <- object enemies <?> "a valid target!"
    w      <- weapon weapons
    return $ Attack target w 

go :: Parser Action
go = do
    try $ abbrev "go"
    spaces
    dir <- direction
    return $ Move dir

lookats = ["house", "piano"]

-- opWord can be used to assert words that may or may not be there
-- ex: look [at] piano. [at] is optional in this case
opWord :: String -> Parser ()
opWord s = optional $ string s *> spaces 


look :: Parser Action
look = do
    try $ abbrev "look"
    --a <- optionMaybe $ spaces *> (optional $ string "at" *> spaces) *> object lookats
    a <- optionMaybe $ spaces *> opWord "at" *> object lookats
    return $ Look a


command :: Parser Action
command = attack <|> go <|> look


type Objects = [String]

enemies, weapons :: Objects
enemies  = [ "goblin"
     , "orc"
     ]

weapons = [ "sword"
        , "axe"
        ]

main :: IO ()
main = do
    input <- getLine
    v <- runParserT command () "error" input
    case v of
      (Left e) -> print e -- putStr "Unknown command: " >> putStrLn input
      (Right v) -> print v
    main

