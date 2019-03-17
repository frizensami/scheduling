module Lib
    ( runSJF
    ) where

import Data.List
import Data.Ord

type ArrivalTime = Double
type ExecutionTime = Double
type RemainingTime = Double
type CurrentTime = Double
data Job = Job { name :: String, arrival :: ArrivalTime, execution :: ExecutionTime, remaining :: RemainingTime } deriving (Show, Eq)
type InputJobs = [Job]
type Schedule = [Job]

job1 = Job "1" 0 3 3
job2 = Job "2" 1 2.5 2.5
job3 = Job "3" 1 1 1
job4 = Job "4" 2 3 3
job5 = Job "5" 3 2 2

runSJF :: IO ()
runSJF = do putStr "\n\n*********STARTING SJF*********\n" 
            -- mapM_ print $ sjf [job1, job2, job3, job4, job5] 0
            printSchedule schedule startTime
            putStrLn "\n------- CALCULATING STATS -------\n"
            totalTurnaround <- printAndGetTurnaround schedule startTime startTurnaround
            putStrLn $ "\nTotal turnaround: " ++ show totalTurnaround
            putStrLn $ "\nAvg turnaround = " ++  show (totalTurnaround / fromIntegral (length schedule)) ++ "\n"
            where schedule = sjf [job1, job2, job3, job4, job5] startTime
                  startTime = 0
                  startTurnaround = 0

-- Runs all jobs in SJF and create schedule of jobs - which job ran when
sjf :: InputJobs -> CurrentTime -> Schedule
sjf [] _ = []
sjf jobs curTime = jobToRun { remaining = remaining jobToRun - jobRunTiming} : sjf restJobs (curTime + jobRunTiming)
    where 
        cursched = sjfScheduleOneJob jobs curTime
        jobToRun = fst cursched
        jobRunTiming = execution jobToRun
        restJobs = snd cursched

-- Return tuple of next job to run and rest of jobs (assumes always have >= 1 job to run at any time)
sjfScheduleOneJob :: [Job] -> CurrentTime -> (Job, [Job])
sjfScheduleOneJob jobs curTime = (nextJob, restJobs) where
    validJobs = filter (\job -> arrival job <= curTime) jobs
    nextJob  = minimumBy (comparing execution) validJobs
    restJobs = [ x | x <- jobs, x /= nextJob ] 


-- PRINTING FUNCTIONS


printSchedule :: Schedule  -> CurrentTime -> IO()
printSchedule [] curTime = putStr $ "Completion Time: " ++ show curTime ++ "\n"
printSchedule schedule curTime = do putStr $ "Time: " ++ show curTime ++ " - job started execution, its state after execution is\n"
                                    print $ head schedule
                                    putStr "\n"
                                    printSchedule (tail schedule) (curTime + (execution . head) schedule)

printAndGetTurnaround :: Schedule  -> CurrentTime -> Double -> IO Double
printAndGetTurnaround [] curTime turnaround = do 
                                                putStr $ "Completion Time: " ++ show curTime 
                                                return turnaround
printAndGetTurnaround schedule curTime turnaround = 
    if jobIsCompleted 
        then do print jobToRun
                putStr $ "t = " ++ show curTime ++ " ==> Turnaround: " ++ show  curTurnaround ++ "\n\n"
                printAndGetTurnaround restJobs (curTime + jobRunTiming) (turnaround + curTurnaround)
        else printAndGetTurnaround restJobs (curTime + jobRunTiming) turnaround
        where 
              jobToRun = head schedule
              restJobs = tail schedule
              jobRunTiming = execution jobToRun
              jobIsCompleted = remaining jobToRun == 0
              curTurnaround = curTime - arrival jobToRun + execution jobToRun



 




-- Runs all jobs in SJF and prints output
{- 
sjf :: [Job] -> CurrentTime -> IO()
sjf [] curtime = putStr $ "t = " ++ show curtime ++ "   (DONE)\n"
sjf jobs curtime = do putStr $ "t = " ++ show curtime ++ "\n"
                      print jobToRun
                      putStr "\n"
                      sjf   restJobs (curtime + jobRunTiming)
    where 
        cursched = sjfScheduleOneJob jobs
        jobToRun = fst cursched
        jobRunTiming = execution jobToRun
        restJobs = snd cursched
-}

