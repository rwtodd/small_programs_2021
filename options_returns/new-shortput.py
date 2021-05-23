#!/bin/env python

from datetime import date, datetime, timedelta
import math

# some constants
TODAY        = date.today()
DAYS_IN_YEAR = 261

def weekdays_between(d1: date, d2: date) -> int:
    "Weekdays between two dates, inclusive"
    day_count = 1 + (d2 - d1).days
    wday = d1.weekday()
    ceil = math.ceil
    # wday 5 is Saturday, 6 is Sunday, so +1/+2 to get to 7
    return day_count + ceil((wday+1)/7) - ceil((wday+1+day_count)/7) + \
                       ceil((wday+2)/7) - ceil((wday+2+day_count)/7)

def annualized(strike: float, prem: float, days: int) -> float:
    return 100 * math.pow(1.0 + (prem / strike), DAYS_IN_YEAR / days) - 100

def parse_date(d: str) -> date:
    """Parse a date loosely, which could be given as '' == TODAY, or
    dd == day dd of the current month, or
    mm-dd == day dd of month mm of the current year, or
    yyyy-mm-dd == the given date."""
    if d == '': return TODAY
    provided =  [int(part) for part in d.split('-')]
    if len(provided) == 2:
        provided = [TODAY.year] + provided
    elif len(provided) == 1:
        provided = [TODAY.year, TODAY.month] + provided
    elif len(provided) > 3:
        raise ValueError('Bad Date format! ' + d)
    return date(*provided)

if __name__ == '__main__':
    FRIDAY = TODAY + timedelta(((4 - TODAY.weekday()) + 7) % 7)
    import argparse
    parser = argparse.ArgumentParser(description='Determine the annualized value of an option premium.')
    parser.add_argument('strike', type=float, help='the strike price')
    parser.add_argument('premium', type=float, help='the premium collected')
    parser.add_argument('-e','--expires', type=parse_date, default=FRIDAY, help='the expiration date (defaults to this friday)')
    parser.add_argument('-o','--open', type=parse_date, default=TODAY, help='the date the position is opened (defaults to today)')
    args = parser.parse_args()
    days = weekdays_between(args.open, args.expires)
    strike, prem = args.strike, args.premium
    result = annualized(strike, prem, days)
    print(f'{result:.2f}%  # Strike price: ${strike}  Premium: ${prem}  Days in market: {days}')

