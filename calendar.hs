import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List.Split (chunksOf)
import Data.List
import Data.Time.Format

daysList = [1..31]
weekDays = ["Su","Mo","Tu","We","Th","Fr","Sa"]
allMonths = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

printWeekDays :: Int -> IO ()
printWeekDays 7 = return ()
printWeekDays n =
  do
    putStr $  (weekDays!!(n) ++ " ")
    printWeekDays(n+1)


printDays :: Int -> Int -> Int -> IO ()
printDays n numDays firstDayNum
  | n > numDays =
    return ()
  | n == 1 && firstDayNum == 7 =
  do
    putStr " "  --if first day is Sunday, spacing is one less
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum
  | (n>1) && (n<10) && (n + firstDayNum -1)`mod` 7 == 0 =
  do --newline after each week. n>1 since if starts on Sunday, no newline needed
    putStr "\n"
    putStr " "  --since still in single digit days, need this space
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum
  | (n>=10) && (n + firstDayNum -1)`mod` 7 == 0 =
  do --newline after each week. since in double digit days, no space needed, unlike above
    putStr "\n"
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum
  | n < 10 =
  do
    putStr "  " --since in single digit days, need this spacing
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum
  | otherwise =
  do
    putStr " "  --since in double digit days, need less spacing
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum


printFirstDaySpacing :: Int -> IO ()
printFirstDaySpacing 1 = return ()  --if down to 1, no more spaces needed
printFirstDaySpacing 8 = return ()  --if Sunday, don't need to move at all
printFirstDaySpacing 2 =
  do
    putStr $ "  "
printFirstDaySpacing n =
  do
    putStr $ "   "
    printFirstDaySpacing (n-1)

daysInMonth :: Integer -> Int -> Int
daysInMonth year month = gregorianMonthLength year month

--returns integer of first day of the month of specified year. Monday = 1 .. Sunday = 7
firstDayOfMonth :: Integer -> Int -> Int
firstDayOfMonth x y = read (formatTime defaultTimeLocale "%u" (fromGregorian x y 01)) :: Int

printCalendar :: Integer -> Int -> IO ()
printCalendar year chosenMonth =
  if chosenMonth < 1 || chosenMonth > 12
    then putStrLn $ "Please choose a month 1 thru 12"
    else do
      putStr "\n      Calendar By:\n      Chance Penner\n\n"
      putStrLn $ "\t" ++ allMonths!!(chosenMonth-1)
      printWeekDays 0
      putStrLn ""
      printFirstDaySpacing ((firstDayOfMonth year chosenMonth) + 1)
      printDays 1 (daysInMonth year chosenMonth) (firstDayOfMonth year chosenMonth)
      putStrLn "\n"
