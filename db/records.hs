module Records (
    Article(..),
    Author(..),
    Stats(..),
    Review(..),
    Reader(..),
    ReaderArticle(..)
) where 

data Article = Article { 
    artId :: Integer,                     
    artName :: String,
    authFId :: Integer,
    statsFId :: Integer,
    artText :: String
} deriving (Eq, Show, Read)

data Author = Author {  
    authId :: Integer,
    authName :: String,
    authAge :: Integer
} deriving (Eq, Show, Read)

data Stats = Stats {    
    statsId :: Integer,
    view :: Integer,
    likes :: Integer,
    dislikes :: Integer
} deriving (Eq, Show, Read)

data Review = Review {
    revId :: Integer,
    articleFId :: Integer,
    revText :: String
} deriving (Eq, Show, Read)

data Reader = Reader {
    readerId :: Integer,
    readerName :: String,
    readerAge :: Integer
} deriving (Eq, Show, Read)

data ReaderArticle = ReaderArticle {
    readArtId :: Integer,
    readerForeignId :: Integer,
    articleForeignId :: Integer
} deriving (Eq, Show, Read)
