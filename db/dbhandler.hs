{-# LANGUAGE MultiWayIf #-}

module DbHandler (
    init,
    flattenedInitAll,
    insertArticle,
    insertAuthor,
    insertStats,
    insertReview,
    insertReader,
    insertReaderArticle,
    viewContentOf,
    dropTableIfExists,
    deleteRow
) where

import Records
import Database.HDBC
import Database.HDBC.Sqlite3
import Prelude hiding (init) 

tableArticle = "Article"
tableAuthor = "Author"
tableStats = "Stats"
tableReview = "Review"
tableReader = "Reader"
tableReaderArticle = "Reader_Article"

createArticleCommand = "CREATE TABLE " ++ tableArticle ++ " (id INTEGER NOT NULL, name TEXT, a_text TEXT, author_id INTEGER, stats_id INTEGER)"
createAuthorCommand = "CREATE TABLE " ++ tableAuthor ++ " (id INTEGER NOT NULL, name TEXT, age INTEGER)"
createStatsCommand = "CREATE TABLE " ++ tableStats ++ " (id INTEGER NOT NULL, view INTEGER, likes INTEGER, dislikes INTEGER)"
createReviewCommand = "CREATE TABLE " ++ tableReview ++ " (id INTEGER NOT NULL, article_id INTEGER, r_text TEXT)"
createReaderCommand = "CREATE TABLE " ++ tableReader ++ " (id INTEGER NOT NULL, name TEXT, age INTEGER)"
createReaderArticleCommand = "CREATE TABLE " ++ tableReaderArticle ++ " (id INTEGER NOT NULL, reader_id INTEGER, article_id INTEGER)"


insertArticle :: (IConnection conn) => (conn, [String]) -> IO Integer
insertArticle (conn, [articleStr]) = 
                let a = read articleStr :: Article
                in run conn ("INSERT INTO " ++ tableArticle ++ " VALUES (?, ?, ?, ?, ?)") 
                [toSql $ artId a, toSql $ artName a, toSql $ authFId a, toSql $ statsFId a, toSql $ artText a]  

insertAuthor :: (IConnection conn) => (conn, [String]) -> IO Integer
insertAuthor (conn, [authorStr]) =
                let a = read authorStr :: Author
                in run conn ("INSERT INTO " ++ tableAuthor ++ " VALUES (?, ?, ?)")
                [toSql $ authId a, toSql $ authName a, toSql $ authAge a]

insertStats :: (IConnection conn) => (conn, [String]) -> IO Integer
insertStats (conn, [statsStr]) =
                let s = read statsStr :: Stats
                in run conn ("INSERT INTO " ++ tableStats ++ " VALUES (?, ?, ?, ?)")
                [toSql $ statsId s, toSql $ view s, toSql $ likes s, toSql $ dislikes s]

insertReview :: (IConnection conn) => (conn, [String]) -> IO Integer
insertReview (conn, [reviewStr]) =
                let r = read reviewStr :: Review
                in run conn ("INSERT INTO " ++ tableReview ++ " VALUES (?, ?, ?)")
                [toSql $ revId r, toSql $ articleFId r, toSql $ revText r]

insertReader :: (IConnection conn) => (conn, [String]) -> IO Integer
insertReader (conn, [readerStr]) =
                let r = read readerStr :: Reader
                in run conn ("INSERT INTO " ++ tableReader ++ " VALUES (?, ?, ?)")
                [toSql $ readerId r, toSql $ readerName r, toSql $ readerAge r]

insertReaderArticle :: (IConnection conn) => (conn, [String]) -> IO Integer
insertReaderArticle (conn, [readerArticleStr]) =
                let ra = read readerArticleStr :: ReaderArticle
                in run conn ("INSERT INTO " ++ tableReaderArticle ++ " VALUES (?, ?, ?")
                [toSql $ readArtId ra, toSql $ readerForeignId ra, toSql $ articleForeignId ra]

viewContent :: (IConnection conn) => (conn, [String]) -> IO [[SqlValue]]
viewContent (conn, [tableName]) = quickQuery' conn  ("SELECT * FROM " ++ tableName) []
        
viewContentOf :: (IConnection conn) => (conn, [String]) -> IO Integer
viewContentOf (conn, [tableName]) = do 
                                        unwrapped <- viewContent (conn, [tableName])
                                        putStrLn $ flatInsertNewlines unwrapped
                                        return 1

flatInsertNewlines :: [[SqlValue]] -> String
flatInsertNewlines [] = []
flatInsertNewlines (value:tail) = show value ++ "\n" ++ flatInsertNewlines tail
                                        
dropTableIfExists :: (IConnection conn) => (conn, [String]) -> IO Integer
dropTableIfExists (conn, [tableName]) = run conn ("DROP TABLE IF EXISTS " ++ tableName) []

init :: (IConnection conn) => (conn, [String]) -> IO Integer
init (conn, [tableName]) = if | tableName == tableArticle -> run conn createArticleCommand []
                                | tableName == tableAuthor -> run conn createAuthorCommand []
                                | tableName == tableStats -> run conn createStatsCommand []
                                | tableName == tableReview -> run conn createReviewCommand []
                                | tableName == tableReader -> run conn createReaderCommand []
                                | tableName == tableReaderArticle -> run conn createReaderArticleCommand []
                                | otherwise -> return $ negate 1

initAll :: (IConnection conn) => (conn, [String]) -> [IO Integer]
initAll (conn, []) = []
initAll (conn, tableName:tail) = init (conn, [tableName]):initAll(conn, tail) 
                                    
flattenedInitAll :: (IConnection conn) => (conn, [String]) -> IO Integer
flattenedInitAll (conn, tables) = do 
                                    unwrapped <- sequence $ initAll (conn, tables)
                                    return $ sum unwrapped 

deleteRow :: (IConnection conn) => (conn, [String]) -> IO Integer
deleteRow (conn, [tableName, matcherName, matcherValue]) = 
    run conn ("DELETE FROM " ++ tableName ++ " where " ++ matcherName ++ "=" ++ matcherValue) []