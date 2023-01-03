const UPPER: usize = 28123;

fn is_abundant(n : usize) -> bool {
    let mut s = 0;
    for x in 1..n {
        if n % x == 0 {
            s = s + x;
        }
    }
    // s == n is perfect
    // s < n is deficient
    return s > n;
}


fn main() {
    let mut abundant = Vec::new();
    for x in 1..UPPER {
        if is_abundant(x) {
           abundant.push(x);
        }
    }

    let mut nonabs: [bool; UPPER] = [true; UPPER];

    for x in abundant.iter() {
      for y in abundant.iter() {
          let s = x + y;
          if s < UPPER {
              nonabs[s] = false;
          }
      }
    }

    let mut total = 0;
              
    for x in 1..UPPER {
      if nonabs[x] {
        println!("{} cannot be written as sum of non-abundant numbers", x);
        total = total + x;
      }
    }
    println!("---8<-------------------------");
    println!("Total sum = {}", total);

}
