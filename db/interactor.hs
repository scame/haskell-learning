import System.Environment
import Database.HDBC
import DbHandler 
import Database.HDBC.Sqlite3
import Prelude hiding (init)

dispatch :: (IConnection conn) => [(String, (conn, [String]) -> IO Integer )]
dispatch = [
            ("init", init),
            ("initAll", flattenedInitAll),
            ("insertArticle", insertArticle),
            ("insertAuthor", insertAuthor),
            ("insertStats", insertStats),
            ("insertReview", insertReview),
            ("insertReaderArticle", insertReaderArticle),
            ("viewContentOf", viewContentOf),
            ("drop", dropTableIfExists),
            ("deleteRow", deleteRow)
            ]

main = do
    conn <- connectSqlite3 "test1.db"
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch 
    executed <- action (conn, args)
    commit conn
    disconnect conn

    