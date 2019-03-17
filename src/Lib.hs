module Lib
    ( runSJF
    ) where

import Data.List
import Data.Ord

type ArrivalTime = Double
type ExecutionTime = Double
type RemainingTime = Double
type CurrentTime = Double
data Job = Job { name :: String, arrivalTime :: ArrivalTime, executionTime :: ExecutionTime, remainingTime:: RemainingTime } deriving (Show, Eq)
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
sjf jobs curTime = 
    let 
        -- Get the job that has to be run and the remaining list of jobs
        (jobToRun, restJobs) = sjfScheduleOneJob jobs curTime
        -- Note how long this job needs to complete
        jobRunTime = executionTime jobToRun
    in
        -- Set the job to completed, add it to the result schedule
        jobToRun { remainingTime = remainingTime jobToRun - jobRunTime } : sjf restJobs (curTime + jobRunTime)

-- Return tuple of next job to run and rest of jobs (assumes always have >= 1 job to run at any time)
sjfScheduleOneJob :: [Job] -> CurrentTime -> (Job, [Job])
sjfScheduleOneJob jobs curTime =
    let
        -- Any job that arrives AFTER current time are invalid to schedule, since
        -- they "don't exist" yet
        validJobs = filter (\job -> arrivalTime job <= curTime) jobs
        -- The next job to execute is the one with the smallest execution time
        nextJob  = minimumBy (comparing executionTime) validJobs
        -- Delete this picked job from the overall list of jobs
        restJobs = [ x | x <- jobs, x /= nextJob ] 
    in 
        (nextJob, restJobs)


-- PRINTING / STATS FUNCTIONS

-- Printing times when each job was run and its status after each run
printSchedule :: Schedule  -> CurrentTime -> IO()
printSchedule [] curTime = putStr $ "Completion Time: " ++ show curTime ++ "\n"
printSchedule schedule curTime = do putStr $ "Time: " ++ show curTime ++ " - job started execution, its state after execution is\n"
                                    print $ head schedule
                                    putStr "\n"
                                    printSchedule (tail schedule) (curTime + (executionTime . head) schedule)

-- Won't work for SRTF in current form - adds the execution time of the job directly (what if pre-empted?)
printAndGetTurnaround :: Schedule  -> CurrentTime -> Double -> IO Double
printAndGetTurnaround [] curTime turnaround = putStr ("Completion Time: " ++ show curTime) >> return turnaround
printAndGetTurnaround schedule curTime turnaround = 
    if jobIsCompleted 
        then do print jobToRun
                putStr $ "t = " ++ show curTime ++ " ==> Turnaround: " ++ show  curTurnaround ++ "\n\n"
                printAndGetTurnaround restJobs (curTime + jobRunTiming) (turnaround + curTurnaround)
        else printAndGetTurnaround restJobs (curTime + jobRunTiming) turnaround
        where 
              jobToRun = head schedule
              restJobs = tail schedule
              jobRunTiming = executionTime jobToRun
              jobIsCompleted = remainingTime jobToRun == 0
              curTurnaround = curTime - arrivalTime jobToRun + executionTime jobToRun