mod tokenizer;
mod tokentype;

fn main() {
    let mut str = "if(1 + 1 >= 2) { foo(); }".chars();

    while let Some(token) = tokenizer::read_token(&mut str) {
        match token {
            tokentype::TokenType::Whitespace => {}
            _ => println!("{:?}", token)
        }
    }
}
