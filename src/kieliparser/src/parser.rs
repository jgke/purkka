use std::iter::Peekable;

macro_rules! maybe_read_token {
    ($iter:ident, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            $iter.next();
            true
        } else {
            false
        }
    }
}

macro_rules! read_token {
    ($iter:ident, $tok:path) => {
        if let Some($tok(..)) = $iter.peek() {
            *$iter.next().unwrap()
        } else {
            unexpected_token($iter.next(), $iter)
        }
    }
}


#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Identifier(usize),

    Assign(),
    Colon(),
    SemiColon(),

    Pub(),
    Const(),
    Static(),
    Fun(),
    Let(),

    Include(),
    IncludeC(),
}

grammar! {
    S -> TranslationUnit;

    TranslationUnit
       -> Leaf. Unit #Token::SemiColon
        | List. Unit #Token::SemiColon TranslationUnit
        ;

    Unit
       -> Declaration
        | IncludeFile
        ;

    Declaration
       -> Declaration. Visibility #Token::Let Mutability #Token::Identifier MaybeType
        | Definition. Visibility #Token::Let Mutability #Token::Identifier MaybeType #Token::Assign AssignmentExpression
        @ #[derive(Clone, Debug, PartialEq)]
        pub enum Declaration {
            Declaration(bool, bool, usize, Option<Type>, Option<AssignmentExpression>)
        }
        ;

    MaybeType
       -> Type. #Token::Colon Type
        | NoType. Epsilon
        ;

    Type
       -> #Token::Identifier
        ;

    MaybeAssign
       -> #Token::Assign AssignmentExpression
        | Epsilon
        ;

    Mutability
       -> Mutable. #Token::Let
        | Const. #Token::Const
        ;

    Visibility
       -> Public. #Token::Pub
        | Private. Epsilon
        ;

    IncludeFile
       -> Normal. #Token::Include Path
        | C. #Token::IncludeC Path
        ;

    AssignmentExpression
       -> #Token::Identifier
       ;

    Path -> #Token::Identifier;
}

pub type Iter<'a, 'b> = &'a mut Peekable<std::slice::Iter<'b, Token>>;

fn unexpected_token(t: Option<&Token>, _iter: Iter) -> ! {
    match t {
        None => panic!("Unexpected end of file"),
        Some(t) => panic!("Unexpected token: {:?}", t),
    }
}

pub fn parse(iter: Iter) -> S {
    S::TranslationUnit(parse_translation_unit(iter))
}

fn parse_translation_unit(iter: Iter) -> TranslationUnit {
    let unit = parse_unit(iter);
    let sc = read_token!(iter, Token::SemiColon);
    let has_more = iter.peek().is_some();
    match has_more {
        true => TranslationUnit::List(unit, sc, Box::new(parse_translation_unit(iter))),
        false => TranslationUnit::Leaf(unit, sc)
    }
}

fn parse_unit(iter: Iter) -> Unit {
    match_first!(
        iter.peek() => _t,
        default unexpected_token(_t.map(|t| *t), iter),

        Declaration => Unit::Declaration(parse_declaration(iter)),
        IncludeFile => Unit::IncludeFile(parse_include(iter)),
    )
}

fn parse_declaration(iter: Iter) -> Declaration {
    let visible = maybe_read_token!(iter, Token::Pub);
    let mutable = match iter.next() {
        Some(Token::Let()) => true,
        Some(Token::Const()) => false,
        t => unexpected_token(t, iter)
    };
    let ident = match iter.next() {
        Some(Token::Identifier(s)) => s,
        t => unexpected_token(t, iter)
    };
    let ty = match_first!(
        iter.peek() => _t,
        default None,

        MaybeType => Some(parse_type(iter)),
    );

    match iter.peek() {
        Some(Token::Assign()) => {
            iter.next();
            let expr = parse_assignment_expr(iter);
            Declaration::Declaration(visible, mutable, *ident, ty, Some(expr))
        }
        _ => Declaration::Declaration(visible, mutable, *ident, ty, None)
    }
}

fn parse_type(iter: Iter) -> Type {
    panic!();
}

fn parse_include(iter: Iter) -> IncludeFile {
    panic!("{:?}", iter);
}

fn parse_assignment_expr(iter: Iter) -> AssignmentExpression {
    panic!("{:?}", iter);
}
