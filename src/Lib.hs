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

-- Change these settings to adjust the jobs and when the scheduler start running
job1 = Job "1" 0 3 3
job2 = Job "2" 1 2.5 2.5
job3 = Job "3" 1 1 1
job4 = Job "4" 2 3 3
job5 = Job "5" 3 2 2
jobs = [job1, job2, job3, job4, job5]
startTime = 0 -- When the scheduler begins to run, offset in units from t = 0

-- Runs and prints jobs in SJF order (only considers jobs that arrived before current time)
runSJF :: IO ()
runSJF = do putStr "\n\n*********STARTING SJF*********\n" 
            printSchedule schedule startTime -- Gets the output schedule from SJF
            putStrLn "\n------- CALCULATING STATS -------\n"
            totalTurnaround <- printAndGetTurnaround schedule startTime startTurnaround
            putStrLn $ "\nTotal turnaround: " ++ show totalTurnaround
            putStrLn $ "\nAvg turnaround = " ++  show (totalTurnaround / fromIntegral (length schedule)) ++ "\n"
            where schedule = sjf jobs startTime
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
sjfScheduleOneJob jobs curTime = (nextJob, restJobs)
     where
        validJobs = filter (\job -> arrival job <= curTime) jobs
        nextJob  = minimumBy (comparing execution) validJobs
        restJobs = [ x | x <- jobs, x /= nextJob ] 


-- PRINTING / STATS FUNCTIONS

-- Printing times when each job was run and its status after each run
printSchedule :: Schedule  -> CurrentTime -> IO()
printSchedule [] curTime = putStr $ "Completion Time: " ++ show curTime ++ "\n"
printSchedule schedule curTime = do putStr $ "Time: " ++ show curTime ++ " - job started execution, its state after execution is\n"
                                    print $ head schedule
                                    putStr "\n"
                                    printSchedule (tail schedule) (curTime + (execution . head) schedule)

-- Won't work for SRTF in current form - adds the execution time of the job directly
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