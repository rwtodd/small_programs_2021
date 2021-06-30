import Foundation
import ArgumentParser

// ----------------------------------------------------------------------
// Some global configuration
// ----------------------------------------------------------------------
let calendar          = Calendar(identifier: .gregorian)
let PER_YEAR : Double = 261
let TODAY             = calendar.startOfDay(for: Date())


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
func parseDate(_ string: String) throws -> Date {
    let formatter = DateFormatter()
    formatter.defaultDate = TODAY

    formatter.dateFormat = "yyyy-MM-dd"
    if let date = formatter.date(from: string) {
        return date
    }

    formatter.dateFormat = "MM-dd"
    if let date = formatter.date(from: string) {
        return date
    }

    throw ValidationError("Format as yyyy-MM-dd or MM-dd!")
}

struct OptionReturns: ParsableCommand {
    @Option(name: .shortAndLong, help: "the strike price.")
    var strike: Double

    @Option(name: .shortAndLong, help: "the premium collected")
    var premium: Double

    @Option(name: .shortAndLong, help: "the expiration date", transform: parseDate)
    var expires: Date?

    @Option(name: .shortAndLong, help: "the date the position was opened", transform: parseDate)
    var openDate: Date?

    mutating func run() throws {
        let date1 = openDate ?? calendar.startOfDay(for:Date()),
            date2 = expires ?? friday(after:date1),
            timeInMarket = weekdaysBetween(date1,date2),
            annualized = 100.0 * pow(1.0 + (premium / strike), PER_YEAR / Double(timeInMarket)) - 100.0
        print("\(String(format: "%.2f",annualized))% # Strike \(strike), Premium \(premium), \(timeInMarket) days in market")
    }
}

OptionReturns.main()

// vim: set expandtab shiftwidth=4 :
