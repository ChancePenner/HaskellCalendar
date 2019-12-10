import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List.Split (chunksOf)
import Data.List
import Data.Time.Format


data Days = Su | Mo | Tu | We | Th | Fr | Sa
           deriving (Show, Eq, Ord, Enum)

data ListOfMonths = January | February | March
          | April   | May      | June
          | July    | August   | September
          | October | November | December
            deriving (Show, Eq, Ord, Enum)

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
  | n == numDays =
  do
    putStr " "
    putStr $ show (daysList!!(n-1))
  | (n<10) && (n + firstDayNum -1)`mod` 7 == 0 =
  do
    putStr "\n"
    putStr " "
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum
  | (n + firstDayNum -1)`mod` 7 == 0 =
  do
    putStr "\n"
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum
  | n < 10 =
  do
    putStr "  "
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum
  | otherwise =
  do
    putStr " "
    putStr $ show (daysList!!(n-1))
    printDays (n+1) numDays firstDayNum


printFirstDaySpacing :: Int -> IO ()
printFirstDaySpacing 0 = return ()
printFirstDaySpacing n =
  do
    putStr $ "  "
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
      putStrLn $ show (firstDayOfMonth year chosenMonth)

      putStrLn $ "\t" ++ allMonths!!(chosenMonth-1)
      printWeekDays 0
      putStrLn ""
      printFirstDaySpacing ((firstDayOfMonth year chosenMonth) + 1)
      printDays 1 (daysInMonth year chosenMonth) (firstDayOfMonth year chosenMonth)
      putStrLn "\n"
