// Thirty days has September,
// April, June and November.
// All the rest have thirty-one,
// Saving February alone,
// Which has twenty-eight, rain or shine.
// And on leap years, twenty-nine.
// jan = 0, feb = 1, mar = 2, apr, = 3, may = 4, jun = 5,
// jul = 6, aug = 7, sep = 8, oct = 9, nov = 10, dec = 11
fn monthdays(m: u32, leap: bool) -> u32 {
    if m == 1 && leap {
        return 29;
    }
    if m == 1 {
        return 28;
    }
    if [8, 3, 5, 10].iter().any(|&x| x == m) {
        return 30
    }
    return 31;
}

// A leap year occurs on any year evenly divisible by 4, 
// but not on a century unless it is divisible by 400.
fn is_leap(y: u32) -> bool {
  if y % 4 == 0 {
      if y % 100 == 0 && y % 400 != 0 {
          return false;
      } else {
          return true;
    } 
  } else {
        return false;
  }
}

fn main() {
  let mut w = 1; // 1 jan 1901 was Tuesday
  let mut sunday1st = 0;
  for y in 1901..=2000 {
      for m in 0..=11 {
        for d in 0..monthdays(m, is_leap(y)) {
            if w == 6 && d == 0 {
                sunday1st += 1;
            }
            w = (w + 1) % 7;
        }
      }
  }
  println!("{} sundays on the 1st", sunday1st);
}
