fn d(n: u128) -> u128 {
    let mut vec = Vec::new();
    for x in 1..n {
        if n % x == 0 {
            vec.push(x);
        }
    }
    return vec.iter().sum();
}

fn main() {
    let const n = 300;
    let mut d_cache: [u128; n] = [0; n];
    for x in 1..n {
        d_cache[x] = d(x);
    }

    for x in 1..n {
      for y in 1..x {
          if x != y && d_cache[x] == y && d_cache[y] == x {
              println!("amicable {} {}", x, y);
              s += x;
              s += y;
          }
      }
    }
}
