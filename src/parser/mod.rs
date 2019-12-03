use ordered_float::OrderedFloat;
use pest::error::{Error as ParseError, ErrorVariant};
use pest::iterators::{Pair, Pairs};
use pest::{prec_climber::*, Parser};

pub use ast::*;

mod ast;

pub type ParseResult<T> = Result<T, pest::error::Error<Rule>>;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct KetamineParser;

pub fn recursive_parse(pair: Pair<Rule>) -> ParseResult<AST> {
    match pair.as_rule() {
        Rule::ident => parse_ident(pair).map(AST::Ident),
        Rule::full_ident => parse_full_ident(pair).map(AST::FullIdent),
        Rule::number => parse_number(pair).map(OrderedFloat::from).map(AST::Number),
        Rule::string => parse_string(pair).map(AST::String),
        Rule::var => parse_var(pair).map(AST::Var),
        Rule::expression => Ok(eval_expr(pair.into_inner())),
        Rule::function => parse_function(pair).map(AST::Function),
        Rule::call => parse_call(pair).map(AST::Call),
        Rule::code => parse_code(pair).map(AST::Code),
        Rule::array => parse_array(pair).map(AST::Array),
        Rule::if_condition => parse_if(pair).map(AST::If),
        Rule::index => parse_index(pair).map(AST::Index),
        Rule::return_ => parse_scope_break(pair).map(AST::BreakScope),
        Rule::object => parse_object(pair).map(AST::Object),
        Rule::boolean_true => Ok(AST::Boolean(true)),
        Rule::boolean_false => Ok(AST::Boolean(false)),
        Rule::null => Ok(AST::Null),
        Rule::assignment => parse_assignment(pair).map(AST::Assignment),
        Rule::for_each => parse_for_each(pair).map(AST::ForEach),
        Rule::while_loop => parse_while(pair).map(AST::While),
        Rule::break_ => parse_break(pair).map(AST::BreakScope),
        v => {
            let variant = ErrorVariant::CustomError {
                message: format!("unexpected pair: {:?}", v),
            };
            Err(ParseError::new_from_span(variant, pair.as_span()))
        }
    }
}

lazy_static! {
    static ref PREC_CLIMBER: PrecClimber<Rule> = {
        use Assoc::*;
        use Rule::*;

        let comp = Operator::new(op_eq, Left)
            | Operator::new(op_not_eq, Left)
            | Operator::new(op_gt, Left)
            | Operator::new(op_lt, Left)
            | Operator::new(op_ge, Left)
            | Operator::new(op_le, Left);

        PrecClimber::new(vec![
            comp,
            Operator::new(op_mod, Left),
            Operator::new(op_add, Left) | Operator::new(op_sub, Left),
            Operator::new(op_mul, Left) | Operator::new(op_div, Left),
            Operator::new(op_pow, Right),
        ])
    };
}

pub fn eval_expr(expression: Pairs<Rule>) -> AST {
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
                _ => unreachable!(),
            };
            let operation = BinaryOperation(Box::new(lhs), operator, Box::new(rhs));
            AST::BinaryOp(operation)
        },
    )
}

pub fn parse_source(src: &str) -> ParseResult<AST> {
    let file = KetamineParser::parse(Rule::FILE, src)?.next().unwrap();
    Ok(AST::Code(parse_code(file)?))
}

pub fn parse_code(pair: Pair<Rule>) -> ParseResult<Code> {
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

pub fn parse_ident(pair: Pair<Rule>) -> ParseResult<Ident> {
    assert_eq!(pair.as_rule(), Rule::ident);
    Ok(Ident(pair.as_str().to_owned()))
}

pub fn parse_full_ident(pair: Pair<Rule>) -> ParseResult<FullIdent> {
    assert_eq!(pair.as_rule(), Rule::full_ident);
    let mut segments = vec![];
    for segment in pair.into_inner() {
        let parsed = parse_ident(segment)?;
        segments.push(parsed);
    }
    Ok(FullIdent(segments))
}

pub fn parse_number(pair: Pair<Rule>) -> ParseResult<f64> {
    assert_eq!(pair.as_rule(), Rule::number);
    pair.as_str().parse().map_err(|e| {
        let variant = ErrorVariant::CustomError {
            message: format!("could not parse number: {}", e),
        };
        ParseError::new_from_span(variant, pair.as_span())
    })
}

pub fn parse_string(pair: Pair<Rule>) -> ParseResult<String> {
    assert_eq!(pair.as_rule(), Rule::string);
    let inner = pair.into_inner().next().unwrap();
    assert_eq!(inner.as_rule(), Rule::__string);
    Ok(inner.as_str().to_owned())
}

pub fn parse_var(pair: Pair<Rule>) -> ParseResult<Var> {
    assert_eq!(pair.as_rule(), Rule::var);
    let mut inner = pair.into_inner();
    let ident = parse_ident(inner.next().unwrap())?;
    let value = recursive_parse(inner.next().unwrap())?;
    Ok(Var(ident, Box::new(value)))
}

pub fn parse_function(pair: Pair<Rule>) -> ParseResult<Function> {
    assert_eq!(pair.as_rule(), Rule::function);
    let mut inner = pair.into_inner();
    let ident = parse_ident(inner.next().unwrap())?;
    let mut params = vec![];

    if inner.peek().unwrap().as_rule() == Rule::function_parameters {
        let params_pair = inner.next().unwrap();
        for pair in params_pair.into_inner() {
            params.push(parse_ident(pair)?)
        }
    }

    let code = parse_code(inner.next().unwrap())?;
    Ok(Function {
        ident,
        params,
        code,
    })
}

pub fn parse_call(pair: Pair<Rule>) -> ParseResult<Call> {
    assert_eq!(pair.as_rule(), Rule::call);
    let mut inner = pair.into_inner();
    let ident = parse_full_ident(inner.next().unwrap())?;
    let mut args = vec![];

    if let Some(args_pair) = inner.next() {
        assert_eq!(args_pair.as_rule(), Rule::call_arguments);
        for arg in args_pair.into_inner() {
            args.push(recursive_parse(arg)?);
        }
    }

    Ok(Call { ident, args })
}

pub fn parse_array(pair: Pair<Rule>) -> ParseResult<Array> {
    assert_eq!(pair.as_rule(), Rule::array);
    let mut array = vec![];
    for inner in pair.into_inner() {
        array.push(recursive_parse(inner)?)
    }
    Ok(Array(array))
}

pub fn parse_if(pair: Pair<Rule>) -> ParseResult<If> {
    fn parse_clause(pair: Pair<Rule>) -> ParseResult<IfClause> {
        let rule = pair.as_rule();
        assert!(
            rule == Rule::if_clause || rule == Rule::else_if_clause || rule == Rule::else_clause
        );

        let mut inner = pair.into_inner();
        let condition = if rule == Rule::else_clause {
            Box::new(AST::Boolean(true))
        } else {
            Box::new(recursive_parse(inner.next().unwrap())?)
        };
        let code = parse_code(inner.next().unwrap())?;
        Ok(IfClause { condition, code })
    }

    assert_eq!(pair.as_rule(), Rule::if_condition);
    Ok(pair
        .into_inner()
        .map(parse_clause)
        .collect::<ParseResult<Vec<IfClause>>>()
        .map(If)?)
}

pub fn parse_index(pair: Pair<Rule>) -> ParseResult<Index> {
    assert_eq!(pair.as_rule(), Rule::index);
    let mut inner = pair.into_inner();
    let receiver = Box::new(recursive_parse(inner.next().unwrap())?);
    let index = Box::new(recursive_parse(inner.next().unwrap())?);
    Ok(Index { receiver, index })
}

pub fn parse_scope_break(pair: Pair<Rule>) -> ParseResult<Option<Box<AST>>> {
    assert!(
        pair.as_rule() == Rule::return_,
        pair.as_rule() == Rule::break_
    );
    let mut inner = pair.into_inner();
    if let Some(value) = inner.next() {
        Ok(Some(Box::new(recursive_parse(value)?)))
    } else {
        Ok(None)
    }
}

pub fn parse_object(pair: Pair<Rule>) -> ParseResult<Object> {
    assert_eq!(pair.as_rule(), Rule::object);
    let mut object = vec![];
    for kv in pair.into_inner() {
        assert_eq!(kv.as_rule(), Rule::kv);
        let mut kv = kv.into_inner();
        let ident = parse_ident(kv.next().unwrap())?;
        let value = recursive_parse(kv.next().unwrap())?;
        object.push((ident, value));
    }
    Ok(Object(object))
}

pub fn parse_assignment(pair: Pair<Rule>) -> ParseResult<Assignment> {
    assert_eq!(pair.as_rule(), Rule::assignment);
    let mut inner = pair.into_inner();
    let ident = parse_full_ident(inner.next().unwrap())?;
    let value = recursive_parse(inner.next().unwrap())?;
    Ok(Assignment(ident, Box::new(value)))
}

pub fn parse_for_each(pair: Pair<Rule>) -> ParseResult<ForEach> {
    assert_eq!(pair.as_rule(), Rule::for_each);
    let mut inner = pair.into_inner();
    let binding = parse_ident(inner.next().unwrap())?;
    let iterable = Box::new(recursive_parse(inner.next().unwrap())?);
    let code = parse_code(inner.next().unwrap())?;
    Ok(ForEach {
        binding,
        iterable,
        code,
    })
}

pub fn parse_while(pair: Pair<Rule>) -> ParseResult<While> {
    assert_eq!(pair.as_rule(), Rule::while_loop);
    let mut inner = pair.into_inner();
    let condition = recursive_parse(inner.next().unwrap())?.into();
    let code = parse_code(inner.next().unwrap())?;
    Ok(While { condition, code })
}

pub fn parse_break(pair: Pair<Rule>) -> ParseResult<Option<Box<AST>>> {
    assert_eq!(pair.as_rule(), Rule::break_);
    let mut inner = pair.into_inner();
    if let Some(value) = inner.next() {
        Ok(Some(Box::new(recursive_parse(value)?)))
    } else {
        Ok(None)
    }
}
