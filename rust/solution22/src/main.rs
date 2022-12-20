use std::fs;

fn cvalue(c : char) -> u32 {
    return (c as u32) - ('A' as u32) + 1;
}

fn svalue(s : &String) -> u32 {
    return s.chars()
            .fold(0, |acc, ch| acc + cvalue(ch));
}

fn main() {
  let contents = fs::read_to_string("names.txt")
        .expect("Should have been able to read the names.txt file");

  let mut vec: Vec<String> =
      contents.split(',')
      .into_iter()
      .map(|s| s.trim().replace("\"", ""))
      .collect::<Vec<String>>();

  vec.sort();

  let mut pos = 1;
  let mut total = 0;
  for x in vec {
      let score = pos * svalue(&x);
      total = total + score;
      println!("{pos}: {x} ({score})");
      pos = pos + 1;
  }
  println!("total = {total}");
}
