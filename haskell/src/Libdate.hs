module Libdate where
import Libnum

data Month     = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
                 deriving (Eq, Show, Ord)
data Date      = Date {dayOfWeek :: Day, day :: Integer, month :: Month, year :: Integer}
                 deriving (Eq)
data Day       = Mon | Tue | Wed | Thu | Fri | Sat | Sun
                 deriving (Eq,Show)

instance Show Date where
    show date@(Date{ dayOfWeek=dow, day=day, month=month,year=year}) 
         = show dow ++ " " ++ show day ++ " " ++ show month ++ " " ++ show year

instance Ord Date where
  (Date {day=day,month=month,year=year}) `compare` (Date {day=day',month=month',year=year'})
    | year > year'   = GT
    | year < year'   = LT
    | month > month' = GT
    | month < month' = LT
    | day > day'     = GT
    | day < day'     = LT
    | otherwise      = EQ

-- A leap year occurs on any year evenly divisible by 4, but not on a century 
-- unless it is divisible by 400.
leapYear Date {year=year} = 4 `divides` year && (100 `notDivides` year || 400 `divides` year)

-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
daysInMonth date@(Date{month=month})
    | (month == Sep) || month == Apr || month == Jun || month == Nov = 30
    | month == Feb && leapYear date                                  = 29
    | month == Feb                                                   = 28
    | otherwise                                                      = 31

nextMonth month
    | month == Jan = Feb
    | month == Feb = Mar
    | month == Mar = Apr
    | month == Apr = May
    | month == May = Jun
    | month == Jun = Jul
    | month == Jul = Aug
    | month == Aug = Sep
    | month == Sep = Oct
    | month == Oct = Nov
    | month == Nov = Dec
    | month == Dec = Jan

nextDay day
    | day == Mon = Tue
    | day == Tue = Wed
    | day == Wed = Thu
    | day == Thu = Fri
    | day == Fri = Sat
    | day == Sat = Sun
    | day == Sun = Mon

nextDate date@(Date{dayOfWeek=dow,  day=day,  month=month,  year=year})
             = Date{dayOfWeek=dow', day=day', month=month', year=year'}
    where dow'                                 = nextDay dow
          day'                                 = (day + 1) `mod1` (daysInMonth date)
          month'
            | day' > day                       = month
            | otherwise                        = nextMonth month
          year'
            | month /= month' && month' == Jan = year + 1
            | otherwise                        = year

-- 1 Jan 1900 was a Monday.
dateList = dateList' Date{dayOfWeek=Mon, day=1, month=Jan, year=1900}
    where dateList' d = d : dateList' (nextDate d)

isFirstOfMonth Date{day=day}       = day == 1
isSunday       Date{dayOfWeek=dow} = dow == Sun
