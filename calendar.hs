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

allMonths = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

--returns integer of first day of the month. Monday = 1 .. Sunday = 7
firstDayOfMonth :: Integer -> Int -> Int
firstDayOfMonth x y = read (formatTime defaultTimeLocale "%u" (fromGregorian x y 01)) :: Int

printCalendar :: Integer -> Int -> IO ()
printCalendar year chosenMonth =
  if chosenMonth < 1 || chosenMonth > 12
    then putStrLn $ "Please choose a month 1 thru 12"
    else do
      putStrLn $ show (firstDayOfMonth year chosenMonth)
      putStrLn ""
