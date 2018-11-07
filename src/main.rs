mod tokenizer;

fn main() {
    let mut str = String::from("1+++++1");

    while let Some((token, next_str)) = tokenizer::read_token(str) {
        println!("{:?}", token);
        str = next_str;
    }
}
