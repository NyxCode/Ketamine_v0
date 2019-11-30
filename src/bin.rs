#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

use pest::{Parser, RuleType, prec_climber::*};
use pest::iterators::{Pair, Pairs};
use pest::error::{Error as ParseError, ErrorVariant};
use serde::Serialize;
use serde_json::error::ErrorCode::Message;

#[derive(Parser)]
#[grammar = "grammar.pest"]
struct KetamineParser;

#[derive(Clone, Debug, Serialize)]
struct Ident(String);

#[derive(Clone, Debug, Serialize)]
struct FullIdent(Vec<Ident>);

#[derive(Clone, Debug, Serialize)]
struct FunctionParameters(Vec<Ident>);

#[derive(Clone, Debug, Serialize)]
struct Code(Vec<AST>);

#[derive(Clone, Debug, Serialize)]
struct Array(Vec<AST>);

#[derive(Clone, Debug, Serialize)]
struct Var(Ident, Box<AST>);

#[derive(Clone, Debug, Serialize)]
struct Function {
    ident: Ident,
    params: Vec<Ident>,
    code: Code,
}

#[derive(Clone, Debug, Serialize)]
struct Call {
    ident: FullIdent,
    args: Vec<AST>,
}

#[derive(Clone, Debug, Serialize)]
struct If {
    condition: Box<AST>,
    code: Code,
}

#[derive(Clone, Debug, Serialize)]
struct Index {
    receiver: Box<AST>,
    index: Box<AST>,
}

#[derive(Clone, Debug, Serialize)]
enum BinaryOperator {
    Eq,
    Gt,
    Lt,
    Ge,
    Le,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    NotEq,
    Mod,
}

#[derive(Clone, Debug, Serialize)]
struct BinaryOperation(Box<AST>, BinaryOperator, Box<AST>);

#[derive(Clone, Debug, Serialize)]
enum AST {
    Ident(Ident),
    FullIdent(FullIdent),
    Number(f64),
    String(String),
    Array(Array),
    Var(Var),
    Function(Function),
    Call(Call),
    Index(Index),
    If(If),
    BinaryOp(BinaryOperation),
    Code(Code),
}

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Rule::*;
        use Assoc::*;

        let comp = Operator::new(op_eq, Left) |
                   Operator::new(op_not_eq, Left) |
                   Operator::new(op_gt, Left) |
                   Operator::new(op_lt, Left) |
                   Operator::new(op_ge, Left) |
                   Operator::new(op_le, Left);


        PrecClimber::new(vec![
            comp,
            Operator::new(op_mod, Left),
            Operator::new(op_add, Left) | Operator::new(op_sub, Left),
            Operator::new(op_mul, Left) | Operator::new(op_div, Left),
            Operator::new(op_pow, Right),
        ])
    };
}

fn eval(expression: Pairs<Rule>) -> AST {
    PREC_CLIMBER.climb(
        expression,
        |pair: Pair<Rule>| recursive_parse(pair).unwrap(),
        |lhs: AST, op: Pair<Rule>, rhs: AST| {
            let operator = match op.as_rule() {
                Rule::op_add => BinaryOperator::Add,
                Rule::op_sub => BinaryOperator::Sub,
                Rule::op_mul => BinaryOperator::Mul,
                Rule::op_div => BinaryOperator::Div,
                Rule::op_pow => BinaryOperator::Pow,
                Rule::op_eq => BinaryOperator::Eq,
                Rule::op_not_eq => BinaryOperator::NotEq,
                Rule::op_gt => BinaryOperator::Gt,
                Rule::op_lt => BinaryOperator::Lt,
                Rule::op_ge => BinaryOperator::Ge,
                Rule::op_le => BinaryOperator::Le,
                Rule::op_mod => BinaryOperator::Mod,
                _ => unreachable!()
            };
            let operation = BinaryOperation(Box::new(lhs), operator, Box::new(rhs));
            AST::BinaryOp(operation)
        },
    )
}

type AstParseResult = Result<AST, pest::error::Error<Rule>>;
type ParseResult<T> = Result<T, pest::error::Error<Rule>>;

fn parse_source(src: &str) -> AstParseResult {
    let file = KetamineParser::parse(Rule::FILE, src)?.next().unwrap();
    println!("{:#?}", file);
    Ok(AST::Code(parse_code(file)?))
}

fn parse_code(pair: Pair<Rule>) -> ParseResult<Code> {
    assert!(pair.as_rule() == Rule::code || pair.as_rule() == Rule::FILE);
    let mut code = vec![];
    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::EOI {
            break;
        }
        let parsed = recursive_parse(inner)?;
        code.push(parsed)
    }
    Ok(Code(code))
}

fn parse_ident(pair: Pair<Rule>) -> ParseResult<Ident> {
    assert_eq!(pair.as_rule(), Rule::ident);
    Ok(Ident(pair.as_str().to_owned()))
}

fn parse_full_ident(pair: Pair<Rule>) -> ParseResult<FullIdent> {
    assert_eq!(pair.as_rule(), Rule::full_ident);
    let mut segments = vec![];
    for segment in pair.into_inner() {
        let parsed = parse_ident(segment)?;
        segments.push(parsed);
    }
    Ok(FullIdent(segments))
}

fn parse_number(pair: Pair<Rule>) -> ParseResult<f64> {
    assert_eq!(pair.as_rule(), Rule::number);
    pair.as_str()
        .parse()
        .map_err(|e| {
            let variant = ErrorVariant::CustomError {
                message: format!("could not parse number: {}", e)
            };
            ParseError::new_from_span(variant, pair.as_span())
        })
}

fn parse_string(pair: Pair<Rule>) -> ParseResult<String> {
    assert_eq!(pair.as_rule(), Rule::string);
    let inner = pair.into_inner().next().unwrap();
    assert_eq!(inner.as_rule(), Rule::__string);
    Ok(inner.as_str().to_owned())
}

fn parse_var(pair: Pair<Rule>) -> ParseResult<Var> {
    assert_eq!(pair.as_rule(), Rule::var);
    let mut inner = pair.into_inner();
    let ident = parse_ident(inner.next().unwrap())?;
    let value = recursive_parse(inner.next().unwrap())?;
    Ok(Var(ident, Box::new(value)))
}

fn parse_function(pair: Pair<Rule>) -> ParseResult<Function> {
    assert_eq!(pair.as_rule(), Rule::function);
    let mut inner = pair.into_inner();
    let ident = parse_ident(inner.next().unwrap())?;
    let params = {
        let mut parameters = vec![];
        for pair in inner.next().unwrap().into_inner() {
            parameters.push(parse_ident(pair)?)
        }
        parameters
    };
    let code = parse_code(inner.next().unwrap())?;
    Ok(Function {
        ident,
        params,
        code,
    })
}

fn parse_call(pair: Pair<Rule>) -> ParseResult<Call> {
    assert_eq!(pair.as_rule(), Rule::call);
    let mut inner = pair.into_inner();
    let ident = parse_full_ident(inner.next().unwrap())?;
    let args_pair = inner.next().unwrap();
    assert_eq!(args_pair.as_rule(), Rule::call_arguments);
    let mut args = vec![];
    for arg in args_pair.into_inner() {
        args.push(recursive_parse(arg)?);
    }
    Ok(Call {
        ident,
        args,
    })
}

fn parse_array(pair: Pair<Rule>) -> ParseResult<Array> {
    assert_eq!(pair.as_rule(), Rule::array);
    let mut array = vec![];
    for inner in pair.into_inner() {
        array.push(recursive_parse(inner)?)
    }
    Ok(Array(array))
}

fn parse_if(pair: Pair<Rule>) -> ParseResult<If> {
    assert_eq!(pair.as_rule(), Rule::if_condition);
    let mut inner = pair.into_inner();
    let condition = Box::new(recursive_parse(inner.next().unwrap())?);
    let code = parse_code(inner.next().unwrap())?;
    Ok(If { condition, code })
}

fn parse_index(pair: Pair<Rule>) -> ParseResult<Index> {
    assert_eq!(pair.as_rule(), Rule::index);
    let mut inner = pair.into_inner();
    let receiver = Box::new(recursive_parse(inner.next().unwrap())?);
    let index = Box::new(recursive_parse(inner.next().unwrap())?);
    Ok(Index { receiver, index })
}

fn recursive_parse(pair: Pair<Rule>) -> AstParseResult {
    match pair.as_rule() {
        Rule::ident => parse_ident(pair).map(AST::Ident),
        Rule::full_ident => parse_full_ident(pair).map(AST::FullIdent),
        Rule::number => parse_number(pair).map(AST::Number),
        Rule::string => parse_string(pair).map(AST::String),
        Rule::var => parse_var(pair).map(AST::Var),
        Rule::expression => Ok(eval(pair.into_inner())),
        Rule::function => parse_function(pair).map(AST::Function),
        Rule::call => parse_call(pair).map(AST::Call),
        Rule::code => parse_code(pair).map(AST::Code),
        Rule::array => parse_array(pair).map(AST::Array),
        Rule::if_condition => parse_if(pair).map(AST::If),
        Rule::index => parse_index(pair).map(AST::Index),
        v => {
            let variant = ErrorVariant::CustomError {
                message: format!("unexpected pair: {:?}", v)
            };
            Err(ParseError::new_from_span(variant, pair.as_span()))
        }
    }
}

fn main() -> Result<(), ParseError<Rule>> {
    let ast = parse_source(
        r#"
            1 ^ 2 > 3 % 6;
        "#
    )?;
    println!("{}", serde_json::to_string_pretty(&ast).unwrap());
    Ok(())
}