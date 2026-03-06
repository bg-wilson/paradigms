// Design a function that takes a DateType value (composed of Year, Month, Day), and tells if the value is
// valid.
// For example: date1 and date5 is valid, while date2, 3, and 4 are invalid.
// date1: (year: 2000, month: 2, day: 29)
// date2: (year: 2001, month: 2, day: 29)
// date3: (year: 2000, month: 14, day: 30)
// date4: (year: 2000, month: 6, day: 31)
// date5: (year: 2022, month: 8, day: 31)


// TODO: create a data type: DateType(composed of Year, Month and Day) using
// record type.

type DateType = {Year:int;Month:int;Day:int}


// TODO: build the real records according to the data type we defined(instances
// of DateType.

let date1 = {Year = 2000; Month = 2; Day = 29}
let date2 = {Year = 2001; Month = 2; Day = 29}
let date3 = {Year = 2000; Month = 14; Day = 30}
let date4 = {Year = 2000; Month = 6; Day = 31}
let date5 = {Year = 2022; Month = 8; Day = 31}


let isLeapYear (year:int) = 
    match year with
    | leap when year % 4 <> 0 -> false
    | common when year % 100 <> 0 -> true
    | leap when year % 400 <> 0 -> false
    | _ -> true

let isFebruary (month:int) =
    match month with
    | feb when month = 2 -> true
    | _ -> false

let validDay (days:int) =
    match days with
    | day when days >= 28 && days <= 31 -> true
    | _ -> false

let days31 (month:int) =
    match month with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> true
    | _ -> false

let days30 (month:int) =
    match month with
    | 4 | 6 | 9 | 11 -> true
    | _ -> false
// TODO: Complete the isDateValid function to pass all asserts below.
// Hints: to complete this function you may need to build other functions.
// For example: a function to check if the year is a leap year or not. A function
// for checking the validation of month and so on.
// Try to use matching expression.
let isDateValid (Date: DateType) : bool =
    let days = Date.Day
    let month = Date.Month
    let year = Date.Year

    match month with
    | notValid when year <= 0 || year > 2026 -> false //check valid year
    | notValid when month < 1 || month > 12 -> false //check valid month
    | notValid when validDay days <> true -> false //check valid number of days
    | feb when month = 2 //if february
        -> match year with
            | leap when isLeapYear year && days = 29 -> true //if leap year and days are 29
            | notLeap when days = 28 -> true //if not leap year and days are 28
            | _ -> false

    //we know if the year, the month, and the number of days are all valid AND it's not february, then the month has to have 31 or 30 days
    | valid when days31 days = true -> true 
    | valid when days30 days = true -> true
    | _ -> false //if it's none of the above then it has to be false
    


// TODO: implement this function
// TODO: using the following code to test your result:
let assert1 = isDateValid date1 = true
let assert2 = isDateValid date2 = false
let assert3 = isDateValid date3 = false
let assert4 = isDateValid date4 = false
let assert5 = isDateValid date5 = true
