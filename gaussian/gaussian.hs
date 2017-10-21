import System.Environment
import GHC.Conc (numCapabilities, getNumProcessors)
import Control.Concurrent
import Control.Concurrent.Chan

data PivotStructure = PivotStructure {
    indicesList :: [Integer],
    targRow :: [Float],
    rowsBelow :: [[Float]]
} deriving Show

readMatrix :: [String] -> [[Float]]
readMatrix = map convertToFloatList
             where convertToFloatList str = map read $ words str :: [Float]

forkAndJoin :: Integer -> [[Float]] -> Integer -> [[Float]] -> IO ()
forkAndJoin _ [] _ res = return ()
forkAndJoin _ [lastRow] _ res = print  (show (lastRow : res) ++ " " ++ show (computeXs (lastRow : res) (fromIntegral $ length lastRow - 2) []))
forkAndJoin cores (currRow:below) currRowInd res = do
                                                        let threadsNumber = computeThreadsNumber cores (fromIntegral $ length below)
                                                        let psList = constructPsList threadsNumber currRowInd  (currRow:below) 1
                                                        joinChannel <- newChan
                                                        exec (mapToIO psList joinChannel)
                                                        computedRowsBelow <- repeatChanRead (length psList) joinChannel below
                                                        forkAndJoin cores computedRowsBelow (currRowInd + 1) (currRow : res)

computeXs :: [[Float]] -> Integer -> [Float] -> [Float]
computeXs [] _ _ = [] 
computeXs (currRow:rowsBelow) pointerInd draft =  let   currElem = currRow !! fromIntegral pointerInd                                            
                                                        last =  currRow !! (length currRow - 1) :: Float

                                                        startIndex = fromIntegral pointerInd + 1
                                                        endIndex = fromIntegral $ length currRow - 2
                                                        mapped = zipWith (*) (slice startIndex endIndex currRow) draft
                                                        summed = sum mapped
                                                        rightPart = last - summed
                                                        x = rightPart / currElem
                                                    in  x : computeXs rowsBelow (pointerInd - 1) (x : draft)
                                                where slice from to xs = take (to - from + 1) (drop from xs)
                                              
                                                
                                            

repeatChanRead :: Int -> Chan PivotStructure -> [[Float]] -> IO [[Float]]
repeatChanRead 0 chan updatedMatrix = return updatedMatrix
repeatChanRead number chan originalMatrix = do
                                            psSubpart <- readChan chan
                                            let subpartLen = fromIntegral $ length (indicesList psSubpart) - 1
                                            let updatedMatrix = updateMatrixFromPs originalMatrix psSubpart subpartLen
                                            repeatChanRead (number - 1) chan updatedMatrix

updateMatrixFromPs :: [[Float]] -> PivotStructure -> Integer ->[[Float]]
updateMatrixFromPs matrix ps 0 = matrix
updateMatrixFromPs matrix ps rowsNumber =   let
                                                updateIndex = indicesList ps !! fromInteger rowsNumber
                                                rowToUpdate = rowsBelow ps !! fromInteger (updateIndex - 1)
                                                updatedMatrix = updateRows matrix rowToUpdate (rowsNumber - 1) 0
                                            in updateMatrixFromPs updatedMatrix ps (rowsNumber - 1)

exec :: [IO ()] -> IO ()
exec [] = return ()
exec (action:tail) = do 
                        forkIO action
                        exec tail   

mapToIO :: [PivotStructure] -> Chan PivotStructure -> [IO ()]
mapToIO [] _ = []
mapToIO (ps:psTail) ch = pivot ps ch (Just 0) : mapToIO psTail ch
                                        

constructPsList :: Integer -> Integer ->[[Float]] -> Integer -> [PivotStructure]
constructPsList _ _ [] _ = []
constructPsList threadsNumber targInd rows pointerInd = 
                                                        if pointerInd + eachShouldTake < fromIntegral (length rows) then
                                                            PivotStructure{
                                                                indicesList = targInd : [pointerInd..(fromIntegral $ eachShouldTake + pointerInd - 1)],
                                                                targRow = head rows,
                                                                rowsBelow = slice (fromIntegral pointerInd) (fromIntegral $ pointerInd + eachShouldTake - 1) rows
                                                            } : constructPsList threadsNumber targInd rows (pointerInd + eachShouldTake)
                                                        else 
                                                            [PivotStructure{
                                                                indicesList = targInd : [pointerInd..fromIntegral (length rows) - 1],
                                                                targRow = head rows,
                                                                rowsBelow = slice (fromIntegral pointerInd) (fromIntegral (length rows) - 1) rows
                                                            }]
                                                        where   slice from to xs = take (to - from + 1) (drop from xs)
                                                                eachShouldTake = toInteger (length rows - 1) `div` threadsNumber
                                                                    


pivot :: PivotStructure -> Chan PivotStructure -> Maybe Integer -> IO ()
pivot ps resCh (Just currRInd) = let    targColumnInd = (head $ indicesList ps) :: Integer
                                        currRow = rowsBelow ps !! fromIntegral currRInd
                                        cellFromTarg = targRow ps !! fromIntegral targColumnInd
                                        cellFromCurr = currRow !! fromIntegral targColumnInd

                                        alpha = (cellFromCurr / cellFromTarg) :: Float
                                        multipliedTargRow = multiplyRow (targRow ps) alpha
                                        substractedCurrRow = zipWith (-) currRow multipliedTargRow
                                        isLastRow = length (indicesList ps) == fromIntegral currRInd
                                        rb = rowsBelow ps

                                        updatedPs = PivotStructure{ indicesList = indicesList ps, 
                                                                    targRow = targRow ps, 
                                                                    rowsBelow = updateRows rb substractedCurrRow currRInd 0 }

                                in if isLastRow then pivot updatedPs resCh Nothing
                                                else pivot updatedPs resCh (Just (currRInd + 1))
                        
pivot ps resCh Nothing = writeChan resCh ps  
                            


multiplyRow :: [Float] -> Float -> [Float]
multiplyRow list multiplier = map (* multiplier) list

updateRows :: [[Float]] -> [Float] -> Integer -> Integer -> [[Float]]
updateRows [] row targInd currIndex = []          
updateRows (currRow:rowsBelow) row targIndex currIndex =    let chosenRow = if currIndex == targIndex then row else currRow
                                                            in chosenRow : updateRows rowsBelow row targIndex (currIndex + 1)                 


computeThreadsNumber :: Integer -> Integer -> Integer
computeThreadsNumber cores rowsLeft 
                | rowsLeft < cores      = 1
                | rowsLeft < cores * 2  = 2
                | otherwise             = cores

main = do
    rows <- getArgs
    cores <- getNumProcessors
    forkIO (forkAndJoin (toInteger cores) (readMatrix rows) 0 [])
    threadDelay (1000 * 1000)
