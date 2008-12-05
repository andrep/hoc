module Progress where

import Control.Concurrent   ( Chan, newChan, writeChan, readChan,
                              newMVar, withMVar, takeMVar,
                              forkIO, killThread )
import Control.Exception    ( block )
import Control.Monad        ( when )
import System.IO.Unsafe     ( unsafePerformIO )
import System.Time
import qualified Data.Map as Map

data ProgressReporter
    = NoProgress
    | ProgressChan String (Chan (Int))
    
dummyProgressReporter :: ProgressReporter
dummyProgressReporter = NoProgress

newProgressReporter :: String -> IO ProgressReporter
newProgressReporter msg
    = do
        chan <- newChan
        return $ ProgressChan msg chan

reportProgress NoProgress n = return ()
reportProgress (ProgressChan _ chan) n = writeChan chan n

monitor1 NoProgress n x = x
monitor1 (ProgressChan _ chan) n x
    = unsafePerformIO $ do
        writeChan chan (n)
        return x
        
runShowingProgress :: String -> (ProgressReporter -> IO a) -> IO a
runShowingProgress msg action
    = do
        putStr $ msg ++ "\n\x1b[A"
        chan <- newChan
        reporterThread <- forkIO (reportLoop chan 0 (-1))
        result <- action (ProgressChan undefined chan)
        killThread reporterThread
        putStrLn $ msg ++ ": done."
        return result
    where
        reportLoop chan i reported = do
            n <- readChan chan
            let percent = (i * 100) `div` n
            when (percent > reported) $
                block (putStr $ msg ++ ": " ++ show percent ++ "%\n\x1b[A")
            reportLoop chan (i+1) (max percent reported)

reportProgressForMap pr m = 
        Map.map (monitor1 pr n) m
    where
        n = Map.size m

data ProgressDisplay
    = InitialProgress
    | PercentProgress Int
    | IndeterminateProgress Int
    deriving(Eq)

printProgress alignAt progress msg
    = do
        putStr $ msg
        putStr $ ':' : replicate (alignAt - n - 1) ' '
        putStr $ ' ' : padTo4 percentage
        putStr $ " [" ++ bar
                      ++ "]\n"
    where
        n = length msg
        padTo4 xs = replicate (4 - length xs) ' ' ++ xs
        percentage = case progress of
            PercentProgress percent
                | percent /= 0 -> show percent ++ "%"
            _ -> ""
        bar = case progress of
            InitialProgress -> replicate 25 ' '
            PercentProgress percent
                -> replicate (percent `div` 4) '*'
                ++ replicate (25 - percent `div` 4) ' '
            IndeterminateProgress i
                -> center 25 (show i)
        center n xs = replicate prepad ' ' ++ xs
                   ++ replicate postpad ' '
            where l = length xs
                  prepad = (n - l) `div` 2
                  postpad = n - l - prepad

newtype MultiProgress = MultiProgress (IO ())

dummyMultiProgress = MultiProgress (return ())

openMultiProgress :: [ProgressReporter] -> IO MultiProgress
openMultiProgress reporters'
    = do
        mapM_ (printProgress alignAt InitialProgress . prMsg) reporters
        up n
        
        reporterLock <- newMVar ()
        
        reporterThreads <- mapM (makeReporter reporterLock) $
                           zip [1..] reporters 
        

        return $ MultiProgress $ do
            takeMVar reporterLock
            mapM_ killThread reporterThreads
            mapM_ (printProgress alignAt (PercentProgress 100) . prMsg) reporters
    where
        reporters = [ r | r@(ProgressChan _ _) <- reporters' ]
        alignAt = 50
        prMsg (ProgressChan msg _) = msg
        
        up 0 = return ()
        up n = putStr $ "\x1B[" ++ show n ++ "A"
        down 0 = return ()
        down n = putStr $ "\x1B[" ++ show n ++ "B"
        n = length reporters
        
        makeReporter reporterLock (line, ProgressChan msg chan)
            = forkIO (loop 1 InitialProgress $ TOD 0 0)
            where
                loop i reported reportedTOD = do
                    n <- readChan chan
                    
                    let progress 
                            | n /= 0
                            = PercentProgress $ min 100 $ 
                                (i * 100) `div` n
                            | otherwise
                            = IndeterminateProgress i
                    
                    
                    
                    if (progress /= reported)
                        then do
                            currentTOD <- getClockTime
                            let final = case progress of
                                            PercentProgress 100 -> True
                                            _ -> False
                                factor = 1000000000000
                                timePassed = (p1 - p0) * factor + (m1 - m0)
                                    where
                                        TOD p0 m0 = reportedTOD
                                        TOD p1 m1 = currentTOD
                            
                            if (final || timePassed > factor `div` 10)
                                then do
                                    withMVar reporterLock $ \_ -> block $ do
                                        down (line - 1)
                                        printProgress alignAt progress msg
                                        up line
                                    loop (i+1) progress currentTOD
                                else
                                    loop (i+1) reported reportedTOD
                        else                     
                            loop (i+1) reported reportedTOD

    
closeMultiProgress :: MultiProgress -> IO ()
closeMultiProgress (MultiProgress finish)
    = finish
    
runMultiProgress :: [ProgressReporter] -> IO a -> IO a
runMultiProgress reporters action
    = do
        mp <- openMultiProgress reporters
        result <- action
        closeMultiProgress mp
        return result

class Monitorable a where
    monitor :: ProgressReporter -> a -> a
    
instance Ord a => Monitorable (Map.Map a b) where
    monitor NoProgress m = m
    monitor pr m = let n = Map.size m in Map.map (monitor1 pr n) m


monitorList NoProgress n xs = xs
monitorList pr n xs
        = f xs
    where
        f (x:xs) = monitor1 pr n' $
                    x : take (k-1) xs
                        ++ f (drop (k-1) xs)
        f [] = []
        
        k = max 1 (n `div` 100)
        n' = (n + k-1) `div` k
        
instance Monitorable [a] where
    monitor pr xs = monitorList pr (length xs) xs 
