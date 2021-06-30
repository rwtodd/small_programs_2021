import Foundation

let calendar = Calendar(identifier: .gregorian)

// Weekdays between two dates, inclusive
func weekdaysBetween(_ d1 : Date, _ d2 : Date) -> Int {
    let count = Double(1 + calendar.dateComponents([.day], from: d1, to: d2).day!),
        wday  = Double(calendar.component(.weekday, from: d1) - 1)
    // wday 0 is Sunday, 6 is Saturday so +0/+1 to get to 7
    return Int(count + ceil(wday/7) - ceil((wday+count)/7) + 
                       ceil((wday+1)/7) - ceil((wday+1+count)/7))
}

// Return the date of the next friday after 'd'
func friday(after d: Date) -> Date {
   var friday = DateComponents()
   friday.weekday = 6
   return calendar.nextDate(after: d, matching: friday, 
                            matchingPolicy: .strict, repeatedTimePolicy: .first, 
                            direction: .forward)!
}

// ----------------------------------------------------------------------
// M A I N  P R O G R A M
// ----------------------------------------------------------------------
let PER_YEAR = 261
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"

let date1 = calendar.startOfDay(for:Date()) // formatter.date(from: "2021-06-29")!
let date2 = friday(after:date1)
print(date1.description)
print(date2.description)
let timeInMarket = weekdaysBetween(date1,date2)
print("We'll be in the market \(timeInMarket) days from today...")

