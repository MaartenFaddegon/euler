fn d(n: usize) -> usize {
    let mut vec = Vec::new();
    for x in 1..n {
        if n % x == 0 {
            vec.push(x);
        }
    }
    return vec.iter().sum();
}

fn main() {
    const N: usize = 10000;
    let mut d_cache: [usize; N] = [0; N];
    for x in 1..N {
        d_cache[x] = d(x);
    }

    let mut s = 0;

    for x in 1..N {
      for y in 1..x {
          if x != y && d_cache[x] == y && d_cache[y] == x {
              println!("amicable {} {}", x, y);
              s += x;
              s += y;
          }
      }
    }

    println!("sum(amicable numbers < {}) = {}", N, s);
}
