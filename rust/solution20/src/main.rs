use num::bigint::{BigInt, Sign};

fn main() {
  let mut r: BigInt = BigInt::new(Sign::Plus, vec![1]);
  let n: u32 = 100;
  for x in 1..=n {
    r *= BigInt::new(Sign::Plus, vec![x])
  }
  println!("{}! = {}", n, r);

  let b0: BigInt = BigInt::new(Sign::Plus, vec![0]);
  let b10: BigInt = BigInt::new(Sign::Plus, vec![10]);
  let mut s: BigInt = BigInt::new(Sign::Plus, vec![0]);
  while r > b0 {
      s += &r % &b10;
      r = &r / &b10;
  }

  println!("sum of digits = {}", s);
}
