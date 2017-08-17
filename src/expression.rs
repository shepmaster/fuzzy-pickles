/// This is the shunting yard algorithm (probably modified from the
/// *pure* algorithm). It tracks the previously parsed thing so that
/// the next token looked for is in a limited set and thus the error
/// messages are accurate. In addition to precedence, it is also needed
/// to reduce the total depth of recursion.

use peresil;
use peresil::combinators::*;

use super::*;

pub fn expression<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attributed<Expression>> {
    match expression_shunting_yard(pm, pt) {
        Ok(ShuntCar { value: expr, ept, .. }) => Progress::success(ept, expr),
        Err((failure_point, err)) => Progress::failure(failure_point, err),
    }
}

#[derive(Debug, Copy, Clone)]
enum ExpressionState {
    Prefix, // Also "beginning of expression"
    Infix { was_range: bool },
    Postfix,
    Atom,
    AtomPostfixOnly,
}

fn expression_shunting_yard<'s>(pm: &mut Master<'s>, mut pt: Point<'s>) ->
    ExprResult<'s, ShuntCar<'s, Attributed<Expression>>>
{
    let mut shunting_yard = ShuntingYard::new();
    let mut state = ExpressionState::Prefix;

    loop {
        match state {
            ExpressionState::Prefix |
            ExpressionState::Infix { .. } => {
                let upgrade_ambiguity = match state {
                    ExpressionState::Infix { was_range } => {
                        pm.state.expression_ambiguity.is_ambiguous() && was_range
                    }
                    _ => false
                };

                let next = if upgrade_ambiguity {
                    head_expression_maximally_ambiguous(expression_prefix_or_atom)(pm, pt)
                } else {
                    expression_prefix_or_atom(pm, pt)
                };

                match next {
                    peresil::Progress { status: peresil::Status::Success(op_or_atom), point } => {
                        match op_or_atom {
                            PrefixOrAtom::Prefix(op) => {
                                shunting_yard.add_prefix(pm, op, pt, point)?;
                                state = ExpressionState::Prefix;
                            }
                            PrefixOrAtom::Atom(expr) => {
                                let postfix_only = expr.may_only_be_followed_by_postfix();
                                shunting_yard.add_expression(expr, pt, point);
                                state = if postfix_only {
                                    ExpressionState::AtomPostfixOnly
                                } else {
                                    ExpressionState::Atom
                                };
                            }
                        }
                        pt = point;
                    }
                    peresil::Progress { status: peresil::Status::Failure(_), point } => {
                        return shunting_yard.finish(pm, point);
                    }
                }
            }
            ExpressionState::Postfix |
            ExpressionState::Atom => {
                match expression_infix_or_postfix(pm, pt) {
                    peresil::Progress { status: peresil::Status::Success(infix_or_postfix), point } => {
                        match infix_or_postfix {
                            InfixOrPostfix::Infix(op) => {
                                let was_range = op.is_range();
                                shunting_yard.add_infix(pm, op, pt, point)?;
                                state = ExpressionState::Infix { was_range };
                            }
                            InfixOrPostfix::Postfix(op) => {
                                shunting_yard.add_postfix(pm, op, pt, point)?;
                                state = ExpressionState::Postfix;
                            }
                        }
                        pt = point;
                    }
                    peresil::Progress { status: peresil::Status::Failure(_), point } => {
                        return shunting_yard.finish(pm, point);
                    }
                }
            }
            ExpressionState::AtomPostfixOnly => {
                match operator_postfix(pm, pt) {
                    peresil::Progress { status: peresil::Status::Success(op), point } => {
                        shunting_yard.add_postfix(pm, op, pt, point)?;
                        state = ExpressionState::Postfix;
                        pt = point;
                    }
                    peresil::Progress { status: peresil::Status::Failure(_), point } => {
                        return shunting_yard.finish(pm, point);
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
enum OperatorPrefix {
    Box(Extent),
    Dereference(Extent),
    Negate(Extent),
    Not(Extent),
    RangeExclusive(Extent),
    RangeInclusive(Extent),
    Reference { is_mutable: Option<Extent> },
}

#[derive(Debug)]
enum OperatorInfix {
    Add(Extent),
    AddAssign(Extent),
    Assign(Extent),
    BitwiseAnd(Extent),
    BitwiseAndAssign(Extent),
    BitwiseOr(Extent),
    BitwiseOrAssign(Extent),
    BitwiseXor(Extent),
    BitwiseXorAssign(Extent),
    BooleanAnd(Extent),
    BooleanOr(Extent),
    Div(Extent),
    DivAssign(Extent),
    Equal(Extent),
    GreaterThan(Extent),
    GreaterThanOrEqual(Extent),
    LessThan(Extent),
    LessThanOrEqual(Extent),
    Mod(Extent),
    ModAssign(Extent),
    Mul(Extent),
    MulAssign(Extent),
    NotEqual(Extent),
    RangeExclusive(Extent),
    RangeInclusive(Extent),
    ShiftLeft(Extent),
    ShiftLeftAssign(Extent),
    ShiftRight(Extent),
    ShiftRightAssign(Extent),
    Sub(Extent),
    SubAssign(Extent),
}

impl OperatorInfix {
    fn is_range(&self) -> bool {
        use self::OperatorInfix::*;
        match *self {
            RangeInclusive(..) | RangeExclusive(..) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
enum OperatorPostfix {
    Ascription { typ: Type },
    AsType { typ: Type },
    Call { args: Vec<Attributed<Expression>> },
    FieldAccess { field: FieldName },
    Slice { index: Attributed<Expression> },
    Try(Extent),
}

#[derive(Debug)]
enum OperatorKind {
    Prefix(Attributed<OperatorPrefix>),
    Infix(OperatorInfix),
    Postfix(OperatorPostfix),
}

type Precedence = u8;

impl OperatorKind {
    fn precedence(&self) -> Precedence {
        use self::OperatorKind::*;

        match *self {
            Prefix(_) => 20,
            Infix(OperatorInfix::RangeExclusive(_)) => 9,
            Infix(OperatorInfix::RangeInclusive(_)) => 9,
            Infix(_) => 10,
            Postfix(OperatorPostfix::Call { .. }) => 10,
            Postfix(_) => 20,
        }
    }
}

#[derive(Debug)]
enum PrefixOrAtom {
    Prefix(Attributed<OperatorPrefix>),
    Atom(Attributed<Expression>),
}

fn expression_prefix_or_atom<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, PrefixOrAtom>
{
    pm.alternate(pt)
        .one(map(attributed(operator_prefix), PrefixOrAtom::Prefix))
        .one(map(attributed(expression_atom), PrefixOrAtom::Atom))
        .finish()
}

#[derive(Debug)]
enum InfixOrPostfix {
    Infix(OperatorInfix),
    Postfix(OperatorPostfix),
}

fn expression_infix_or_postfix<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, InfixOrPostfix>
{
    pm.alternate(pt)
        .one(map(operator_infix, InfixOrPostfix::Infix))
        .one(map(operator_postfix, InfixOrPostfix::Postfix))
        .finish()
}

fn operator_prefix<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorPrefix>
{
    pm.alternate(pt)
        .one(map(kw_box, OperatorPrefix::Box))
        // 3 characters
        .one(map(triple_period, OperatorPrefix::RangeInclusive))
        // 2 characters
        .one(map(double_period, OperatorPrefix::RangeExclusive))
        // 1 character
        .one(map(asterisk, OperatorPrefix::Dereference))
        .one(map(bang, OperatorPrefix::Not))
        .one(map(minus, OperatorPrefix::Negate))
        .one(operator_prefix_reference)

        .finish()
}

fn operator_prefix_reference<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorPrefix>
{
    sequence!(pm, pt, {
        _          = ampersand;
        is_mutable = optional(kw_mut);
    }, |_, _| OperatorPrefix::Reference { is_mutable })
}

fn operator_infix<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorInfix>
{
    pm.alternate(pt)
        // 3 characters
        .one(map(shift_left_equals, OperatorInfix::ShiftLeftAssign))
        .one(map(shift_right_equals, OperatorInfix::ShiftRightAssign))
        .one(map(triple_period, OperatorInfix::RangeInclusive))
        // 2 characters
        .one(map(ampersand_equals, OperatorInfix::BitwiseAndAssign))
        .one(map(caret_equals, OperatorInfix::BitwiseXorAssign))
        .one(map(divide_equals, OperatorInfix::DivAssign))
        .one(map(double_ampersand, OperatorInfix::BooleanAnd))
        .one(map(double_equals, OperatorInfix::Equal))
        .one(map(double_left_angle, OperatorInfix::ShiftLeft))
        .one(map(double_period, OperatorInfix::RangeExclusive))
        .one(map(double_pipe, OperatorInfix::BooleanOr))
        .one(map(double_right_angle, OperatorInfix::ShiftRight))
        .one(map(greater_than_or_equals, OperatorInfix::GreaterThanOrEqual))
        .one(map(less_than_or_equals, OperatorInfix::LessThanOrEqual))
        .one(map(minus_equals, OperatorInfix::SubAssign))
        .one(map(not_equal, OperatorInfix::NotEqual))
        .one(map(percent_equals, OperatorInfix::ModAssign))
        .one(map(pipe_equals, OperatorInfix::BitwiseOrAssign))
        .one(map(plus_equals, OperatorInfix::AddAssign))
        .one(map(times_equals, OperatorInfix::MulAssign))
        // 1 character
        .one(map(ampersand, OperatorInfix::BitwiseAnd))
        .one(map(asterisk, OperatorInfix::Mul))
        .one(map(caret, OperatorInfix::BitwiseXor))
        .one(map(equals, OperatorInfix::Assign))
        .one(map(left_angle, OperatorInfix::LessThan))
        .one(map(minus, OperatorInfix::Sub))
        .one(map(percent, OperatorInfix::Mod))
        .one(map(pipe, OperatorInfix::BitwiseOr))
        .one(map(plus, OperatorInfix::Add))
        .one(map(right_angle, OperatorInfix::GreaterThan))
        .one(map(slash, OperatorInfix::Div))
        .finish()
}

fn operator_postfix<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorPostfix>
{
    pm.alternate(pt)
        .one(operator_postfix_as_type)
        .one(operator_postfix_ascription)
        .one(operator_postfix_call)
        .one(operator_postfix_field_access)
        .one(operator_postfix_slice)
        .one(map(question_mark, OperatorPostfix::Try))
        .finish()
}

fn operator_postfix_as_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorPostfix>
{
    sequence!(pm, pt, {
        _   = kw_as;
        typ = typ;
    }, |_, _| OperatorPostfix::AsType { typ })
}

fn operator_postfix_ascription<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorPostfix>
{
    sequence!(pm, pt, {
        _   = colon;
        typ = typ;
    }, |_, _| OperatorPostfix::Ascription { typ })
}

// TODO: avoid recursion here
fn operator_postfix_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorPostfix>
{
    sequence!(pm, pt, {
        _    = left_paren;
        args = head_expression_no_longer_ambiguous(zero_or_more_tailed_values(comma, expression));
        _    = right_paren;
    }, |_, _| OperatorPostfix::Call { args })
}

fn operator_postfix_field_access<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorPostfix>
{
    sequence!(pm, pt, {
        _     = period;
        field = field_name;
    }, |_, _| OperatorPostfix::FieldAccess { field })
}

fn field_name<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, FieldName> {
    pm.alternate(pt)
        .one(map(path_component, FieldName::Path))
        .one(map(field_name_number, FieldName::Number))
        .finish()
}

fn field_name_number<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    number_normal(pm, pt)
        .and_then(pt, |n| n.into_simple().ok_or(Error::ExpectedNumber))
}

// TODO: avoid recursion here
fn operator_postfix_slice<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, OperatorPostfix>
{
    sequence!(pm, pt, {
        _     = left_square;
        index = head_expression_no_longer_ambiguous(expression);
        _     = right_square;
    }, |_, _| OperatorPostfix::Slice { index })
}

fn expression_atom<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    pm.alternate(pt)
        .one(map(expr_if, Expression::If))
        .one(map(expr_if_let, Expression::IfLet))
        .one(map(expr_for_loop, Expression::ForLoop))
        .one(map(expr_loop, Expression::Loop))
        .one(map(expr_while, Expression::While))
        .one(map(expr_while_let, Expression::WhileLet))
        .one(map(expr_match, Expression::Match))
        .one(map(expr_unsafe_block, Expression::UnsafeBlock))
        .one(map(expr_block, Expression::Block))
        .one(map(expr_macro_call, Expression::MacroCall))
        .one(map(expr_let, Expression::Let))
        .one(expr_tuple_or_parenthetical)
        .one(map(expr_array, Expression::Array))
        .one(map(character_literal, Expression::Character))
        .one(map(string_literal, Expression::String))
        .one(map(expr_closure, Expression::Closure))
        .one(map(expr_return, Expression::Return))
        .one(map(expr_continue, Expression::Continue))
        .one(map(expr_break, Expression::Break))
        .one(map(number_literal, Expression::Number))
        .one(map(expr_byte, Expression::Byte))
        .one(map(expr_byte_string, Expression::ByteString))
        .one(map(expr_disambiguation, Expression::Disambiguation))
        .one(map(expr_value, Expression::Value))
        .finish()
}

struct ShuntCar<'s, T> {
    value: T,
    spt: Point<'s>,
    ept: Point<'s>,
}

struct ShuntingYard<'s> {
    operators: Vec<ShuntCar<'s, OperatorKind>>,
    result: Vec<ShuntCar<'s, Attributed<Expression>>>,
}

type PointRange<'s> = std::ops::Range<Point<'s>>;
type ExprResult<'s, T> = std::result::Result<T, (Point<'s>, Error)>;

impl<'s> ShuntingYard<'s> {
    fn new() -> Self {
        ShuntingYard {
            operators: Vec::new(),
            result: Vec::new(),
        }
    }

    fn add_expression(&mut self, expr: Attributed<Expression>, spt: Point<'s>, ept: Point<'s>) {
        //println!("Pushing {:?}", expr);
        self.result.push(ShuntCar { value: expr, spt, ept });
    }

    fn add_prefix(&mut self, pm: &Master, op: Attributed<OperatorPrefix>, spt: Point<'s>, ept: Point<'s>) ->
        ExprResult<'s, ()>
    {
        let op = OperatorKind::Prefix(op);
        self.apply_precedence(pm, &op)?;
        self.operators.push(ShuntCar { value: op, spt, ept });
        Ok(())
    }

    fn add_infix(&mut self, pm: &Master, op: OperatorInfix, spt: Point<'s>, ept: Point<'s>) ->
        ExprResult<'s, ()>
    {
        let op = OperatorKind::Infix(op);
        self.apply_precedence(pm, &op)?;
        self.operators.push(ShuntCar { value: op, spt, ept });
        Ok(())
    }

    fn add_postfix(&mut self, pm: &Master, op: OperatorPostfix, spt: Point<'s>, ept: Point<'s>) ->
        ExprResult<'s, ()>
    {
        let op = OperatorKind::Postfix(op);
        self.apply_precedence(pm, &op)?;
        self.operators.push(ShuntCar { value: op, spt, ept });
        Ok(())
    }

    fn finish(mut self, pm: &Master, failure_point: Point<'s>) ->
        ExprResult<'s, ShuntCar<'s, Attributed<Expression>>>
    {
        //println!("Finishing expression");

        self.apply_all(pm)?;

        let r = self.pop_expression(failure_point);
        assert_eq!(0, self.result.len());
        r
    }

    fn apply_precedence(&mut self, pm: &Master, operator: &OperatorKind) -> ExprResult<'s, ()>  {
        //println!("About to push {:?}", operator);
        let op_precedence = operator.precedence();
        while self.operators.last().map_or(false, |&ShuntCar { value: ref top, .. }| top.precedence() > op_precedence) {
            let ShuntCar { value, spt, ept } = self.operators.pop()
                .expect("Cannot pop operator that was just there");
            //println!("Precedence current ({}) top ({})", value.precedence(), op_precedence);
            self.apply_one(pm, value, spt..ept)?;
        }
        Ok(())
    }

    fn apply_all(&mut self, pm: &Master) -> ExprResult<'s, ()> {
        while let Some(ShuntCar { value, spt, ept }) = self.operators.pop() {
            self.apply_one(pm, value, spt..ept)?;
        }
        Ok(())
    }

    fn apply_one(&mut self, pm: &Master, op: OperatorKind, op_range: PointRange<'s>) ->
        ExprResult<'s, ()>
    {
        use self::OperatorKind::*;

        //println!("Applying {:?}", op);

        match op {
            // TODO: Make into unary ?
            Prefix(Attributed { extent, attributes, value: OperatorPrefix::Dereference(..) }) => {
                self.apply_prefix(pm, op_range, extent, attributes, |extent, expr| {
                    Expression::Dereference(Dereference {
                        extent,
                        target: Box::new(expr),
                        whitespace: Vec::new(),
                    })
                })
            },
            Prefix(Attributed { extent, attributes, value: OperatorPrefix::Reference { is_mutable } }) => {
                self.apply_prefix(pm, op_range, extent, attributes, |extent, expr| {
                    Expression::Reference(Reference {
                        extent,
                        is_mutable,
                        target: Box::new(expr),
                    })
                })
            },
            // TODO: Make into unary ?
            Prefix(Attributed { extent, attributes, value: OperatorPrefix::Box(..) }) => {
                self.apply_prefix(pm, op_range, extent, attributes, |extent, expr| {
                    Expression::Box(ExpressionBox {
                        extent,
                        target: Box::new(expr),
                    })
                })
            },
            Prefix(Attributed { extent, attributes, value: OperatorPrefix::RangeInclusive(..) }) => {
                self.apply_maybe_prefix(pm, op_range, extent, attributes, |extent, expr| {
                    Expression::RangeInclusive(RangeInclusive {
                        extent,
                        lhs: None,
                        rhs: expr.map(Box::new),
                    })
                })
            },
            Prefix(Attributed { extent, attributes, value: OperatorPrefix::RangeExclusive(..) }) => {
                self.apply_maybe_prefix(pm, op_range, extent, attributes, |extent, expr| {
                    Expression::Range(Range {
                        extent,
                        lhs: None,
                        rhs: expr.map(Box::new),
                    })
                })
            },
            Prefix(Attributed { extent, attributes, value: OperatorPrefix::Negate(..) }) => {
                self.apply_unary(pm, op_range, extent, attributes, UnaryOp::Negate)
            },
            Prefix(Attributed { extent, attributes, value: OperatorPrefix::Not(..) }) => {
                self.apply_unary(pm, op_range, extent, attributes, UnaryOp::Not)
            },

            Infix(OperatorInfix::Add(..)) => self.apply_binary(pm, op_range, BinaryOp::Add),
            Infix(OperatorInfix::AddAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::AddAssign),
            Infix(OperatorInfix::Assign(..)) => self.apply_binary(pm, op_range, BinaryOp::Assign),
            Infix(OperatorInfix::BitwiseAnd(..)) => self.apply_binary(pm, op_range, BinaryOp::BitwiseAnd),
            Infix(OperatorInfix::BitwiseAndAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::BitwiseAndAssign),
            Infix(OperatorInfix::BitwiseOr(..)) => self.apply_binary(pm, op_range, BinaryOp::BitwiseOr),
            Infix(OperatorInfix::BitwiseOrAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::BitwiseOrAssign),
            Infix(OperatorInfix::BitwiseXor(..)) => self.apply_binary(pm, op_range, BinaryOp::BitwiseXor),
            Infix(OperatorInfix::BitwiseXorAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::BitwiseXorAssign),
            Infix(OperatorInfix::BooleanAnd(..)) => self.apply_binary(pm, op_range, BinaryOp::BooleanAnd),
            Infix(OperatorInfix::BooleanOr(..)) => self.apply_binary(pm, op_range, BinaryOp::BooleanOr),
            Infix(OperatorInfix::Div(..)) => self.apply_binary(pm, op_range, BinaryOp::Div),
            Infix(OperatorInfix::DivAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::DivAssign),
            Infix(OperatorInfix::Equal(..)) => self.apply_binary(pm, op_range, BinaryOp::Equal),
            Infix(OperatorInfix::GreaterThan(..)) => self.apply_binary(pm, op_range, BinaryOp::GreaterThan),
            Infix(OperatorInfix::GreaterThanOrEqual(..)) => self.apply_binary(pm, op_range, BinaryOp::GreaterThanOrEqual),
            Infix(OperatorInfix::LessThan(..)) => self.apply_binary(pm, op_range, BinaryOp::LessThan),
            Infix(OperatorInfix::LessThanOrEqual(..)) => self.apply_binary(pm, op_range, BinaryOp::LessThanOrEqual),
            Infix(OperatorInfix::Mod(..)) => self.apply_binary(pm, op_range, BinaryOp::Mod),
            Infix(OperatorInfix::ModAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::ModAssign),
            Infix(OperatorInfix::Mul(..)) => self.apply_binary(pm, op_range, BinaryOp::Mul),
            Infix(OperatorInfix::MulAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::MulAssign),
            Infix(OperatorInfix::NotEqual(..)) => self.apply_binary(pm, op_range, BinaryOp::NotEqual),
            Infix(OperatorInfix::ShiftLeft(..)) => self.apply_binary(pm, op_range, BinaryOp::ShiftLeft),
            Infix(OperatorInfix::ShiftLeftAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::ShiftLeftAssign),
            Infix(OperatorInfix::ShiftRight(..)) => self.apply_binary(pm, op_range, BinaryOp::ShiftRight),
            Infix(OperatorInfix::ShiftRightAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::ShiftRightAssign),
            Infix(OperatorInfix::Sub(..)) => self.apply_binary(pm, op_range, BinaryOp::Sub),
            Infix(OperatorInfix::SubAssign(..)) => self.apply_binary(pm, op_range, BinaryOp::SubAssign),

            Infix(OperatorInfix::RangeInclusive(..)) => {
                self.apply_maybe_infix(pm, op_range, |extent, lhs, rhs| {
                    Expression::RangeInclusive(RangeInclusive {
                        extent,
                        lhs: Some(Box::new(lhs)),
                        rhs: rhs.map(Box::new),
                    }).into()
                })
            },
            Infix(OperatorInfix::RangeExclusive(..)) => {
                self.apply_maybe_infix(pm, op_range, |extent, lhs, rhs| {
                    Expression::Range(Range {
                        extent,
                        lhs: Some(Box::new(lhs)),
                        rhs: rhs.map(Box::new),
                    }).into()
                })
            },

            Postfix(OperatorPostfix::FieldAccess { field }) => {
                self.apply_postfix(pm, op_range, |extent, expr| {
                    Expression::FieldAccess(FieldAccess {
                        extent,
                        target: Box::new(expr),
                        field,
                    }).into()
                })
            },
            Postfix(OperatorPostfix::Call { args }) => {
                self.apply_postfix(pm, op_range, |extent, expr| {
                    Expression::Call(Call {
                        extent,
                        target: Box::new(expr),
                        args,
                    }).into()
                })
            },
            Postfix(OperatorPostfix::Slice { index }) => {
                self.apply_postfix(pm, op_range, |extent, expr| {
                    Expression::Slice(Slice {
                        extent,
                        target: Box::new(expr),
                        index: Box::new(index),
                    }).into()
                })
            },
            Postfix(OperatorPostfix::AsType { typ }) => {
                self.apply_postfix(pm, op_range, |extent, expr| {
                    Expression::AsType(AsType {
                        extent,
                        target: Box::new(expr),
                        typ,
                    }).into()
                })
            },
            Postfix(OperatorPostfix::Ascription { typ }) => {
                self.apply_postfix(pm, op_range, |extent, expr| {
                    Expression::Ascription(Ascription {
                        extent,
                        target: Box::new(expr),
                        typ,
                    }).into()
                })
            },
            Postfix(OperatorPostfix::Try(..)) => {
                self.apply_postfix(pm, op_range, |extent, expr| {
                    Expression::TryOperator(TryOperator {
                        extent,
                        target: Box::new(expr),
                    }).into()
                })
            },
        }
    }

    fn apply_maybe_prefix<F>(
        &mut self,
        pm: &Master,
        op_range: PointRange<'s>,
        extent_of_prefix: Extent,
        attributes: Vec<Attribute>,
        f: F
    ) ->
        ExprResult<'s, ()>
        where F: FnOnce(Extent, Option<Attributed<Expression>>) -> Expression
    {
        if self.result.is_empty() {
            let extent_of_inner_expression = pm.state.ex(op_range.start, op_range.end);
            let value = f(extent_of_inner_expression, None);
            let extent = (extent_of_prefix.0, extent_of_inner_expression.1);
            let new_expr = Attributed { extent, attributes, value };
            self.result.push(ShuntCar { value: new_expr, spt: op_range.start, ept: op_range.end });
            Ok(())
        } else {
            self.apply_prefix(pm, op_range, extent_of_prefix, attributes, |extent, expr| f(extent, Some(expr)))
        }
    }

    fn apply_prefix<F>(
        &mut self,
        pm: &Master,
        op_range: PointRange<'s>,
        extent_of_prefix: Extent,
        attributes: Vec<Attribute>,
        f: F
    ) ->
        ExprResult<'s, ()>
        where F: FnOnce(Extent, Attributed<Expression>) -> Expression
    {
        let ShuntCar { value: expr, ept: expr_ept, .. } = self.pop_expression(op_range.start)?;
        let extent_of_inner_expression = pm.state.ex(op_range.start, expr_ept);
        let value = f(extent_of_inner_expression, expr);
        let extent = (extent_of_prefix.0, extent_of_inner_expression.1);
        let new_expr = Attributed { extent, attributes, value };
        self.result.push(ShuntCar { value: new_expr, spt: op_range.start, ept: expr_ept });
        Ok(())
    }

    fn apply_unary(&mut self, pm: &Master, op_range: PointRange<'s>, extent: Extent, attributes: Vec<Attribute>, op: UnaryOp) ->
        ExprResult<'s, ()>
    {
        self.apply_prefix(pm, op_range, extent, attributes, |extent, expr| {
            Expression::Unary(Unary {
                extent,
                op,
                value: Box::new(expr),
                whitespace: Vec::new(),
            })
        })
    }

    fn apply_maybe_infix<F>(&mut self, pm: &Master, op_range: PointRange<'s>, f: F) ->
        ExprResult<'s, ()>
        where F: FnOnce(Extent, Attributed<Expression>, Option<Attributed<Expression>>) -> Expression
    {
        if self.result.len() <= 1 {
            let ShuntCar { value: lhs, spt: lexpr_spt, .. } = self.pop_expression(op_range.end)?;
            let extent = pm.state.ex(lexpr_spt, op_range.end);
            let new_expr = f(extent, lhs, None);
            self.result.push(ShuntCar { value: new_expr.into(), spt: lexpr_spt, ept: op_range.end });
            Ok(())
        } else {
            self.apply_infix(pm, op_range, |extent, lhs, rhs| f(extent, lhs, Some(rhs)))
        }
    }

    fn apply_infix<F>(&mut self, pm: &Master, op_range: PointRange<'s>, f: F) ->
        ExprResult<'s, ()>
        where F: FnOnce(Extent, Attributed<Expression>, Attributed<Expression>) -> Expression
    {
        let ShuntCar { value: rhs, ept: rexpr_ept, .. } = self.pop_expression(op_range.end)?;
        let ShuntCar { value: lhs, spt: lexpr_spt, .. } = self.pop_expression(op_range.start)?;
        let extent = pm.state.ex(lexpr_spt, rexpr_ept);
        let new_expr = f(extent, lhs, rhs);
        self.result.push(ShuntCar { value: new_expr.into(), spt: lexpr_spt, ept: rexpr_ept });
        Ok(())
    }

    fn apply_binary(&mut self, pm: &Master, op_range: PointRange<'s>, op: BinaryOp) ->
        ExprResult<'s, ()>
    {
        self.apply_infix(pm, op_range, |extent, lhs, rhs| {
            Expression::Binary(Binary {
                extent,
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                whitespace: Vec::new(),
            }).into()
        })
    }

    fn apply_postfix<F>(&mut self, pm: &Master, op_range: PointRange<'s>, f: F) ->
        ExprResult<'s, ()>
        where F: FnOnce(Extent, Attributed<Expression>) -> Expression
    {
        let ShuntCar { value: expr, spt, ept: expr_ept } = self.pop_expression(op_range.start)?;
        let ept = max(expr_ept, op_range.end);
        let extent = pm.state.ex(spt, ept);
        let new_expr = f(extent, expr);
        self.result.push(ShuntCar { value: new_expr.into(), spt, ept });
        Ok(())
    }

    fn pop_expression(&mut self, location: Point<'s>) ->
        ExprResult<'s, ShuntCar<'s, Attributed<Expression>>>
    {
        self.result.pop().ok_or((location, Error::ExpectedExpression))
    }
}

pub fn expr_macro_call<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroCall> {
    sequence!(pm, pt, {
        spt  = point;
        name = ident;
        _    = bang;
        arg  = optional(ident);
        args = expr_macro_call_args;
    }, |pm: &mut Master, pt| MacroCall { extent: pm.state.ex(spt, pt), name, arg, args })
}

fn expr_macro_call_args<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MacroCallArgs> {
    pm.alternate(pt)
        .one(map(expr_macro_call_paren, MacroCallArgs::Paren))
        .one(map(expr_macro_call_square, MacroCallArgs::Square))
        .one(map(expr_macro_call_curly, MacroCallArgs::Curly))
        .finish()
}

fn expr_macro_call_paren<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_paren;
        args = parse_nested_until(Token::is_left_paren, Token::is_right_paren);
        _    = right_paren;
    }, |_, _| args)
}

fn expr_macro_call_square<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_square;
        args = parse_nested_until(Token::is_left_square, Token::is_right_square);
        _    = right_square;
    }, |_, _| args)
}

fn expr_macro_call_curly<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Extent> {
    sequence!(pm, pt, {
        _    = left_curly;
        args = parse_nested_until(Token::is_left_curly, Token::is_right_curly);
        _    = right_curly;
    }, |_, _| args)
}

fn expr_let<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Let> {
    sequence!(pm, pt, {
        spt     = point;
        _       = kw_let;
        pattern = pattern;
        typ     = optional(expr_let_type);
        value   = optional(expr_let_rhs);
    }, |pm: &mut Master, pt| Let {
        extent: pm.state.ex(spt, pt),
        pattern,
        typ,
        value: value.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_let_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _   = colon;
        typ = typ;
    }, |_, _| typ)
}

fn expr_let_rhs<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attributed<Expression>> {
    sequence!(pm, pt, {
        _     = equals;
        value = expression;
    }, |_, _| value)
}

fn expr_if<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, If> {
    sequence!(pm, pt, {
        spt               = point;
        _                 = kw_if;
        (condition, body) = expr_followed_by_block;
        more              = zero_or_more(expr_if_else_if);
        else_body         = optional(expr_if_else_end);
    }, move |pm: &mut Master, pt| If {
        extent: pm.state.ex(spt, pt),
        condition: Box::new(condition),
        body: Box::new(body),
        more,
        else_body: else_body.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_if_else_if<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, If> {
    sequence!(pm, pt, {
        _    = kw_else;
        tail = expr_if;
    }, |_, _| tail)
}

fn expr_if_else_end<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Block> {
    sequence!(pm, pt, {
        _         = kw_else;
        else_body = block;
    }, |_, _| else_body)
}

fn expr_followed_by_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, (Attributed<Expression>, Block)> {
    sequence!(pm, pt, {
        condition = control_flow_head_expression(expression);
        body      = block;
    }, |_, _| (condition, body))
}

fn expr_for_loop<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ForLoop> {
    sequence!(pm, pt, {
        spt          = point;
        label        = optional(loop_label);
        _            = kw_for;
        pattern      = pattern;
        _            = kw_in;
        (iter, body) = expr_followed_by_block;
    }, |pm: &mut Master, pt| ForLoop {
        extent: pm.state.ex(spt, pt),
        label,
        pattern,
        iter: Box::new(iter),
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

fn loop_label<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Lifetime> {
    sequence!(pm, pt, {
        lifetime = lifetime;
        _        = colon;
    }, |_, _| lifetime)
}

fn expr_loop<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Loop> {
    sequence!(pm, pt, {
        spt   = point;
        label = optional(loop_label);
        _     = kw_loop;
        body  = block;
    }, |pm: &mut Master, pt| Loop { extent: pm.state.ex(spt, pt), label, body: Box::new(body), whitespace: Vec::new() })
}

fn expr_if_let<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, IfLet> {
    sequence!(pm, pt, {
        spt           = point;
        _             = kw_if;
        _             = kw_let;
        pattern       = pattern;
        _             = equals;
        (value, body) = expr_followed_by_block;
    }, |pm: &mut Master, pt| IfLet {
        extent: pm.state.ex(spt, pt),
        pattern,
        value: Box::new(value),
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

fn expr_while<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, While> {
    sequence!(pm, pt, {
        spt           = point;
        label         = optional(loop_label);
        _             = kw_while;
        (value, body) = expr_followed_by_block;
    }, |pm: &mut Master, pt| While {
        extent: pm.state.ex(spt, pt),
        label,
        value: Box::new(value),
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

fn expr_while_let<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, WhileLet> {
    sequence!(pm, pt, {
        spt           = point;
        label         = optional(loop_label);
        _             = kw_while;
        _             = kw_let;
        pattern       = pattern;
        _             = equals;
        (value, body) = expr_followed_by_block;
    }, |pm: &mut Master, pt| WhileLet {
        extent: pm.state.ex(spt, pt),
        label,
        pattern,
        value: Box::new(value),
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

impl ImplicitSeparator for MatchArm {
    fn is_implicit_separator(&self) -> bool {
        match self.hand {
            MatchHand::Brace(..) => true,
            MatchHand::Expression(..) => false,
        }
    }
}

fn expr_match<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Match> {
    sequence!(pm, pt, {
        spt  = point;
        _    = kw_match;
        head = control_flow_head_expression(expression);
        _    = left_curly;
        arms = zero_or_more_implicitly_tailed_values(comma, match_arm);
        _    = right_curly;
    }, |pm: &mut Master, pt| Match { extent: pm.state.ex(spt, pt), head: Box::new(head), arms, whitespace: Vec::new() })
}

fn match_arm<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MatchArm> {
    sequence!(pm, pt, {
        spt        = point;
        attributes = zero_or_more(attribute);
        pattern    = one_or_more_tailed_values(pipe, pattern);
        guard      = optional(match_arm_guard);
        _          = thick_arrow;
        hand       = match_arm_hand;
    }, |pm: &mut Master, pt| MatchArm { extent: pm.state.ex(spt, pt), attributes, pattern, guard, hand, whitespace: Vec::new() })
}

fn match_arm_guard<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Attributed<Expression>> {
    sequence!(pm, pt, {
        _     = kw_if;
        guard = head_expression_no_longer_ambiguous(expression);
    }, |_, _| guard)
}

fn match_arm_hand<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, MatchHand> {
    pm.alternate(pt)
        .one(map(head_expression_no_longer_ambiguous(expr_block), |b| MatchHand::Brace(Expression::Block(b).into())))
        .one(map(head_expression_no_longer_ambiguous(expression), MatchHand::Expression))
        .finish()
}

fn expr_tuple_or_parenthetical<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Expression> {
    sequence!(pm, pt, {
        spt    = point;
        _      = left_paren;
        values = head_expression_no_longer_ambiguous(zero_or_more_tailed(comma, expression));
        _      = right_paren;
    }, move |pm: &mut Master, pt| {
        let extent = pm.state.ex(spt, pt);
        let values = values;
        let Tailed { mut values, separator_count, .. } = values;
        match (values.len(), separator_count) {
            (1, 0) => Expression::Parenthetical(Parenthetical {
                extent,
                expression: Box::new(values.pop().expect("Must have one parenthesized value")),
            }),
            _ => Expression::Tuple(Tuple {
                extent,
                members: values,
            }),
        }
    })
}

fn expr_array<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Array> {
    pm.alternate(pt)
        .one(map(expr_array_explicit, Array::Explicit))
        .one(map(expr_array_repeated, Array::Repeated))
        .finish()
}

fn expr_array_explicit<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ArrayExplicit> {
    sequence!(pm, pt, {
        spt    = point;
        _      = left_square;
        values = head_expression_no_longer_ambiguous(zero_or_more_tailed_values(comma, expression));
        _      = right_square;
    }, |pm: &mut Master, pt| ArrayExplicit { extent: pm.state.ex(spt, pt), values })
}

fn expr_array_repeated<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ArrayRepeated> {
    sequence!(pm, pt, {
        spt   = point;
        _     = left_square;
        value = head_expression_no_longer_ambiguous(expression);
        _     = semicolon;
        count = expression;
        _     = right_square;
    }, |pm: &mut Master, pt| ArrayRepeated {
        extent: pm.state.ex(spt, pt),
        value: Box::new(value),
        count: Box::new(count),
        whitespace: Vec::new(),
    })
}

pub fn expr_byte<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Byte> {
    byte(pm, pt)
        .map(|extent| Byte { extent, value: Character { extent, value: extent } }) // FIXME: value
}

pub fn expr_byte_string<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ByteString> {
    pm.alternate(pt)
        .one(map(byte_string, |extent| {
            ByteString { extent, value: String { extent, value: extent } }  // FIXME: value
        }))
        .one(map(byte_string_raw, |extent| {
            ByteString { extent, value: String { extent, value: extent } }  // FIXME: value
        }))
        .finish()
}

fn expr_closure<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Closure> {
    sequence!(pm, pt, {
        spt                 = point;
        mov                 = optional(kw_move);
        _                   = pipe;
        args                = zero_or_more_tailed_values(comma, expr_closure_arg);
        _                   = pipe;
        (return_type, body) = expr_closure_return;
    }, |pm: &mut Master, pt| Closure {
        extent: pm.state.ex(spt, pt),
        is_move: mov.is_some(),
        args,
        return_type,
        body: Box::new(body),
        whitespace: Vec::new(),
    })
}

fn expr_closure_arg<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, ClosureArg> {
    sequence!(pm, pt, {
        name = pattern;
        typ  = optional(expr_closure_arg_type);
    }, |_, _| ClosureArg { name, typ, whitespace: Vec::new() })
}

fn expr_closure_arg_type<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Type> {
    sequence!(pm, pt, {
        _   = colon;
        typ = typ;
    }, |_, _| typ)
}

fn expr_closure_return<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, (Option<Type>, Attributed<Expression>)>
{
    pm.alternate(pt)
        .one(expr_closure_return_explicit)
        .one(expr_closure_return_inferred)
        .finish()
}

fn expr_closure_return_explicit<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, (Option<Type>, Attributed<Expression>)>
{
    sequence!(pm, pt, {
        _    = thin_arrow;
        typ  = typ;
        body = expr_closure_return_body;
    }, |_, _| (Some(typ), body.into()))
}

fn expr_closure_return_body<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Expression>
{
    pm.alternate(pt)
        .one(expr_tuple_or_parenthetical)
        .one(map(expr_block, Expression::Block))
        .finish()
}

fn expr_closure_return_inferred<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, (Option<Type>, Attributed<Expression>)>
{
    map(expression, |body| (None, body))(pm, pt)
}

fn expr_return<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Return> {
    sequence!(pm, pt, {
        spt   = point;
        _     = kw_return;
        value = optional(expression);
    }, |pm: &mut Master, pt| Return {
        extent: pm.state.ex(spt, pt),
        value: value.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_continue<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Continue> {
    sequence!(pm, pt, {
        spt   = point;
        _     = kw_continue;
        label = optional(lifetime);
    }, |pm: &mut Master, pt| Continue { extent: pm.state.ex(spt, pt), label, whitespace: Vec::new() })
}

fn expr_break<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Break> {
    sequence!(pm, pt, {
        spt   = point;
        _     = kw_break;
        label = optional(lifetime);
        value = optional(expression);
    }, |pm: &mut Master, pt| Break {
        extent: pm.state.ex(spt, pt),
        label,
        value: value.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Box<Block>> {
    if pm.state.expression_ambiguity == ExpressionAmbiguity::Maximum {
        Progress::failure(pt, Error::BlockNotAllowedHere)
    } else {
        block(pm, pt).map(Box::new)
    }
}

fn expr_unsafe_block<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, UnsafeBlock> {
    sequence!(pm, pt, {
        spt  = point;
        _    = kw_unsafe;
        body = block;
    }, |pm: &mut Master, pt| UnsafeBlock { extent: pm.state.ex(spt, pt), body: Box::new(body), whitespace: Vec::new() })
}

fn expr_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Value> {
    if pm.state.expression_ambiguity.is_ambiguous() {
        sequence!(pm, pt, {
            spt  = point;
            name = pathed_ident;
        }, |pm: &mut Master, pt| Value { extent: pm.state.ex(spt, pt), name, literal: None })
    } else {
        sequence!(pm, pt, {
            spt     = point;
            name    = pathed_ident;
            literal = optional(expr_value_struct_literal);
        }, |pm: &mut Master, pt| Value { extent: pm.state.ex(spt, pt), name, literal })
    }
}

fn expr_value_struct_literal<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructLiteral> {
    sequence!(pm, pt, {
        spt    = point;
        _      = left_curly;
        fields = zero_or_more_tailed_values(comma, expr_value_struct_literal_field);
        splat  = optional(expr_value_struct_literal_splat);
        _      = right_curly;
    }, |pm: &mut Master, pt| StructLiteral {
        extent: pm.state.ex(spt, pt),
        fields,
        splat: splat.map(Box::new),
        whitespace: Vec::new(),
    })
}

fn expr_value_struct_literal_field<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, StructLiteralField> {
    sequence!(pm, pt, {
        spt   = point;
        name  = ident;
        mpt   = point;
        value = optional(expr_value_struct_literal_field_value);
    }, |pm: &mut Master, _| {
        let value = value.unwrap_or_else(|| Expression::Value(Value {
            extent: pm.state.ex(spt, mpt),
            name: name.into(),
            literal: None,
        }).into());
        StructLiteralField { name, value, whitespace: Vec::new() }
    })
}

fn expr_value_struct_literal_field_value<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Attributed<Expression>>
{
    sequence!(pm, pt, {
        _     = colon;
        value = head_expression_no_longer_ambiguous(expression);
    }, |_, _| value)
}

fn expr_value_struct_literal_splat<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
    Progress<'s, Attributed<Expression>>
{
    sequence!(pm, pt, {
        _     = double_period;
        value = head_expression_no_longer_ambiguous(expression);
    }, |_, _| value)
}

fn expr_disambiguation<'s>(pm: &mut Master<'s>, pt: Point<'s>) -> Progress<'s, Disambiguation> {
    sequence!(pm, pt, {
        spt        = point;
        core       = disambiguation_core;
        components = zero_or_more_tailed_values_resume(double_colon, path_component);
    }, move |pm: &mut Master, pt| Disambiguation {
        extent: pm.state.ex(spt, pt),
        from_type: core.from_type,
        to_type: core.to_type,
        components,
        whitespace: core.whitespace,
    })
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ExpressionAmbiguity {
    Unambiguous,
    OnlyStructLiterals,
    Maximum,
}

impl ExpressionAmbiguity {
    fn is_ambiguous(&self) -> bool {
        use self::ExpressionAmbiguity::*;

        match *self {
            Unambiguous => false,
            _ => true,
        }
    }
}

impl Default for ExpressionAmbiguity {
    fn default() -> Self { ExpressionAmbiguity::Unambiguous }
}

fn control_flow_head_expression<'s, F, T>(parser: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    set_ambiguity_level(parser, ExpressionAmbiguity::OnlyStructLiterals)
}

fn head_expression_maximally_ambiguous<'s, F, T>(parser: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    set_ambiguity_level(parser, ExpressionAmbiguity::Maximum)
}

fn head_expression_no_longer_ambiguous<'s, F, T>(parser: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    set_ambiguity_level(parser, ExpressionAmbiguity::Unambiguous)
}

// Constructs like `if foo {}` will greedily match the block as a
// structure literal expression (e.g. `foo { a: 1 }`) and then fail
// because the body isn't found.
//
// Constructs like `for i in 0.. {}` will greedily match the block as
// the second part of the range (e.g. `0..{}`) and then fail because
// the body isn't found.
//
// When we enter the head expression of these control flow
// expressions, we immediately disable struct literals. If we enter
// the RHS of a range, we also disable blocks. They should both be
// re-enabled by entering some kind of enclosing container because it
// is no longer ambiguous.
fn set_ambiguity_level<'s, F, T>(parser: F, level: ExpressionAmbiguity) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        let old = pm.state.expression_ambiguity;
        pm.state.expression_ambiguity = level;

        let res = parser(pm, pt);

        pm.state.expression_ambiguity = old;

        res
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_utils::*;

    #[test]
    fn expression_atom_can_have_attributes() {
        let p = qp(expression, "#[moo] 42");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expression_operator_prefix_can_have_attributes() {
        let p = qp(expression, "#[moo] *42");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn expr_true() {
        let p = qp(expression, "true");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_number_binary() {
        let p = qp(expression, "0x0101");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_number_decimal() {
        let p = qp(expression, "123");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_number_hexadecimal() {
        let p = qp(expression, "0xDEADBEEF");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn expr_number_octal() {
        let p = qp(expression, "0o777");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_number_negative() {
        let p = qp(expression, "-0x1");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_number_with_spacers() {
        let p = qp(expression, "1_000_000");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_let_explicit_type() {
        let p = qp(expression, "let foo: bool");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_let_explicit_type_and_value_not_confused_with_greater_than_or_equal() {
        let p = unwrap_progress(qp(expression, "let foo: Vec<u8>=vec![]"));
        assert!(p.is_let());
        assert_eq!(p.extent(), (0, 23));
    }

    #[test]
    fn expr_let_explicit_type_and_value_not_confused_with_shift_right_assign() {
        let p = unwrap_progress(qp(expression, "let foo: Vec<Vec<u8>>=vec![]"));
        assert!(p.is_let());
        assert_eq!(p.extent(), (0, 28));
    }

    #[test]
    fn expr_let_mut() {
        let p = qp(expression, "let mut pm = Master::new()");
        assert_eq!(unwrap_progress(p).extent(), (0, 26))
    }

    #[test]
    fn expr_let_no_value() {
        let p = qp(expression, "let pm");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_assign() {
        let p = qp(expression, "a = b");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_assign_to_field() {
        let p = qp(expression, "a.b = c");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_value_with_path() {
        let p = qp(expression, "Master::new()");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_field_access_name() {
        let p = qp(expression, "foo.bar");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_field_access_number() {
        let p = qp(expression, "foo.0");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_field_access_multiple() {
        let p = qp(expression, "foo.bar.baz");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_call_function() {
        let p = unwrap_progress(qp(expression, "foo()"));
        assert!(p.is_call());
        assert_eq!(p.extent(), (0, 5))
    }

    #[test]
    fn expr_call_method() {
        let p = unwrap_progress(qp(expression, "foo.bar()"));
        assert!(p.is_call());
        assert_eq!(p.extent(), (0, 9))
    }

    #[test]
    fn expr_call_method_multiple() {
        let p = unwrap_progress(qp(expression, "foo.bar().baz()"));
        assert!(p.is_call());
        assert_eq!(p.extent(), (0, 15))
    }

    #[test]
    fn expr_call_method_with_turbofish() {
        let p = unwrap_progress(qp(expression, "foo.bar::<u8>()"));
        assert!(p.is_call());
        assert_eq!(p.extent(), (0, 15))
    }

    #[test]
    fn expr_call_method_with_turbofish_nested() {
        let p = unwrap_progress(qp(expression, "e.into_iter().collect::<BTreeSet<_>>()"));
        assert!(p.is_call());
        assert_eq!(p.extent(), (0, 38))
    }

    #[test]
    fn expr_call_method_with_turbofish_on_type_and_method() {
        let p = unwrap_progress(qp(expression, "Foo::<u8>::bar()"));
        assert!(p.is_call());
        assert_eq!(p.extent(), (0, 16))
    }

    #[test]
    fn expr_call_of_expr() {
        let p = unwrap_progress(qp(expression, "{foo}()"));
        assert!(p.is_call());
        assert_eq!(p.extent(), (0, 7))
    }

    #[test]
    fn expr_for_loop() {
        let p = qp(expression, "for (a, b) in c {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 18))
    }

    #[test]
    fn expr_for_loop_with_label() {
        let p = qp(expression, "'a: for (a, b) in c {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 22))
    }

    #[test]
    fn expr_for_loop_followed_by_infix() {
        let p = qp(expression, "for a in b {} + c");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_loop() {
        let p = qp(expression, "loop {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_loop_with_label() {
        let p = qp(expression, "'a: loop {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_match() {
        let p = qp(expression, "match foo { _ => () }");
        assert_eq!(unwrap_progress(p).extent(), (0, 21))
    }

    #[test]
    fn expr_match_brace_with_no_comma_followed_by_tuple_isnt_a_function_call() {
        // `_ => {} (_,)` is unambigous from a function call
        // `{foo}(arg)`. We must check blocks specifically.
        let p = qp(expression, "match (1,) { (1,) => {} (_,) => {} }");
        assert_eq!(unwrap_progress(p).extent(), (0, 36))
    }

    #[test]
    fn expr_match_expr_trailing_comma_and_whitespace() {
        let p = qp(expression, "match 1 { 1 => 2, _ => 3, }");
        assert_eq!(unwrap_progress(p).extent(), (0, 27))
    }

    #[test]
    fn expr_match_head_followed_by_block() {
        let p = qp(expression, "match foo {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn expr_tuple() {
        let p = qp(expression, "(1, 2)");
        let t = unwrap_progress(p);
        assert_eq!(t.extent(), (0, 6));
        assert!(t.is_tuple())
    }

    #[test]
    fn expr_tuple_of_none() {
        let p = qp(expression, "()");
        let t = unwrap_progress(p);
        assert_eq!(t.extent(), (0, 2));
        assert!(t.is_tuple())
    }

    #[test]
    fn expr_tuple_of_one() {
        let p = qp(expression, "(1,)");
        let t = unwrap_progress(p);
        assert_eq!(t.extent(), (0, 4));
        assert!(t.is_tuple())
    }

    #[test]
    fn expr_parens() {
        let p = qp(expression, "(a && b)");
        let t = unwrap_progress(p);
        assert_eq!(t.extent(), (0, 8));
        assert!(t.is_parenthetical())
    }

    #[test]
    fn expr_parens_with_one_value_is_not_tuple() {
        let p = qp(expression, "(1)");
        let t = unwrap_progress(p);
        assert_eq!(t.extent(), (0, 3));
        assert!(t.is_parenthetical())
    }

    #[test]
    fn expr_block() {
        let p = qp(expression, "{}");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn expr_unsafe_block() {
        let p = qp(expression, "unsafe {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_if_() {
        let p = qp(expression, "if a {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_if_else() {
        let p = qp(expression, "if a {} else {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn expr_if_else_if() {
        let p = qp(expression, "if a {} else if b {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 20))
    }

    #[test]
    fn expr_if_followed_by_infix() {
        let p = qp(expression, "if a {} + 1");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_if_followed_by_postfix() {
        let p = qp(expression, "if a {}.foo()");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_if_let() {
        let p = qp(expression, "if let Some(a) = None {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 24))
    }

    #[test]
    fn expr_while() {
        let p = qp(expression, "while is_awesome() {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 21))
    }

    #[test]
    fn expr_while_with_label() {
        let p = qp(expression, "'a: while is_awesome() {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 25))
    }

    #[test]
    fn expr_while_let() {
        let p = qp(expression, "while let Some(a) = None {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 27))
    }

    #[test]
    fn expr_while_let_with_label() {
        let p = qp(expression, "'a: while let Some(a) = None {}");
        assert_eq!(unwrap_progress(p).extent(), (0, 31))
    }

    #[test]
    fn expr_binary_op() {
        let p = qp(expression, "a < b");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_binary_multiple() {
        let p = qp(expression, "1 + 2 + 3");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_binary_op_two_char() {
        let p = qp(expression, "a >= b");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_binary_op_equality() {
        let p = qp(expression, "a == b != c");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_binary_op_math() {
        let p = qp(expression, "a + b - c / d % e");
        assert_eq!(unwrap_progress(p).extent(), (0, 17))
    }

    #[test]
    fn expr_binary_op_boolean_logic() {
        let p = qp(expression, "a && b || c");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_binary_op_shifting() {
        let p = qp(expression, "a >> b << c");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_binary_op_shift_assign() {
        let p = qp(expression, "a >>= b <<= c");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_binary_op_bitwise() {
        let p = qp(expression, "a & b | c ^ d");
        assert_eq!(unwrap_progress(p).extent(), (0, 13))
    }

    #[test]
    fn expr_binary_op_bitwise_assign() {
        let p = qp(expression, "a &= b |= c ^= d");
        assert_eq!(unwrap_progress(p).extent(), (0, 16))
    }

    #[test]
    fn expr_braced_true() {
        let p = qp(expression, "{ true }");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_macro_call_with_nested_parens() {
        let p = qp(expression, "foo!(())");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_macro_call_with_quoted_parens() {
        let p = qp(expression, r#"foo!("(")"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_macro_call_with_square_brackets() {
        let p = qp(expression, "vec![]");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_macro_call_with_curly_brackets() {
        let p = qp(expression, "foo! { }");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_macro_call_with_ident() {
        let p = qp(expression, "macro_rules! foo { }");
        assert_eq!(unwrap_progress(p).extent(), (0, 20))
    }

    #[test]
    fn expr_range_both() {
        let p = qp(expression, "1..2");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_range_left() {
        let p = qp(expression, "3..");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_range_right() {
        let p = qp(expression, "..4");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_range_none() {
        let p = qp(expression, "..");
        assert_eq!(unwrap_progress(p).extent(), (0, 2))
    }

    #[test]
    fn expr_range_after_infix() {
        let p = qp(expression, "1 + 2..");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_range_inclusive_both() {
        let p = qp(expression, "1...2");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_range_inclusive_left() {
        let p = qp(expression, "3...");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_range_inclusive_right() {
        let p = qp(expression, "...4");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_range_inclusive_none() {
        let p = qp(expression, "...");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_value_struct_literal() {
        let p = qp(expression, "Point { a: 1 }");
        assert_eq!(unwrap_progress(p).extent(), (0, 14))
    }

    #[test]
    fn expr_value_struct_literal_shorthand() {
        let p = qp(expression, "Point { a }");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_value_struct_literal_with_splat() {
        let p = qp(expression, "Point { x: 1, ..point }");
        assert_eq!(unwrap_progress(p).extent(), (0, 23))
    }

    #[test]
    fn expr_value_starts_with_keyword() {
        let p = qp(expression, "continuez");
        assert_eq!(unwrap_progress(p).extent(), (0, 9));
    }

    #[test]
    fn expr_closure() {
        let p = qp(expression, "|a| a");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_closure_no_args() {
        let p = qp(expression, "|| 42");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_closure_multiple() {
        let p = qp(expression, "|a, b| a + b");
        assert_eq!(unwrap_progress(p).extent(), (0, 12))
    }

    #[test]
    fn expr_closure_explicit_type() {
        let p = qp(expression, "|a: u8| a");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_closure_return_type() {
        let p = qp(expression, "|a| -> u8 { a }");
        assert_eq!(unwrap_progress(p).extent(), (0, 15))
    }

    #[test]
    fn expr_closure_pattern() {
        let p = qp(expression, "|&a| a");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_closure_move() {
        let p = qp(expression, "move || 42");
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn expr_return() {
        let p = qp(expression, "return 1");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_return_no_value() {
        let p = qp(expression, "return");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_continue() {
        let p = unwrap_progress(qp(expression, "continue"));
        assert!(p.is_continue());
        assert_eq!(p.extent(), (0, 8))
    }

    #[test]
    fn expr_continue_with_label() {
        let p = unwrap_progress(qp(expression, "continue 'outer"));
        assert!(p.is_continue());
        assert_eq!(p.extent(), (0, 15))
    }

    #[test]
    fn expr_break() {
        let p = unwrap_progress(qp(expression, "break"));
        assert!(p.is_break());
        assert_eq!(p.extent(), (0, 5))
    }

    #[test]
    fn expr_break_with_label() {
        let p = unwrap_progress(qp(expression, "break 'outer"));
        assert!(p.is_break());
        assert_eq!(p.extent(), (0, 12))
    }

    #[test]
    fn expr_break_with_value() {
        let p = unwrap_progress(qp(expression, "break 42"));
        assert!(p.is_break());
        assert_eq!(p.extent(), (0, 8))
    }

    #[test]
    fn expr_break_with_label_and_value() {
        let p = unwrap_progress(qp(expression, "break 'outer 42"));
        assert!(p.is_break());
        assert_eq!(p.extent(), (0, 15))
    }

    #[test]
    fn expr_array_explicit() {
        let p = qp(expression, "[1, 1]");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_array_repeated() {
        let p = qp(expression, "[1; 2*3]");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_char_literal() {
        let p = qp(expression, "'a'");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_char_literal_escape() {
        let p = qp(expression, r"'\''");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_string_literal() {
        let p = qp(expression, r#""a""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_string_literal_escape() {
        let p = qp(expression, r#""\"""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_string_literal_raw() {
        let p = qp(expression, r###"r#"foo"#"###);
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_slice_index() {
        let p = qp(expression, "a[..2]");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_reference() {
        let p = qp(expression, "&foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_reference_double() {
        let p = qp(expression, "&&foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_reference_mut() {
        let p = qp(expression, "&mut foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_dereference() {
        let p = qp(expression, "*foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_unary_not() {
        let p = qp(expression, "!foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_unary_negate() {
        let p = qp(expression, "-foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_as_type() {
        let p = qp(expression, "42 as u8");
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_as_type_followed_by_addition() {
        let p = unwrap_progress(qp(expression, "42 as u8 + 1"));
        let p = p.value.into_binary().unwrap();
        assert!(p.lhs.is_as_type());
        assert_eq!(p.extent, (0, 12));
    }

    #[test]
    fn expr_as_type_of_value() {
        let p = qp(expression, "bits as u64");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_type_ascription() {
        let p = qp(expression, "42 : u8");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_infix_with_left_hand_prefix_operator() {
        let p = qp(expression, "*a + b");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_infix_with_right_hand_prefix_operator() {
        let p = qp(expression, "a + *b");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_infix_with_left_and_right_hand_prefix_operator() {
        let p = qp(expression, "*a + *b");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_infix_with_left_hand_postfix_operator() {
        let p = qp(expression, "a? + b");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_infix_with_right_hand_postfix_operator() {
        let p = qp(expression, "a + b?");
        assert_eq!(unwrap_progress(p).extent(), (0, 6))
    }

    #[test]
    fn expr_infix_with_left_and_right_hand_postfix_operator() {
        let p = qp(expression, "a? + b?");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_infix_with_left_hand_prefix_and_postfix_operator() {
        let p = qp(expression, "*a? + b");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_infix_with_right_hand_prefix_and_postfix_operator() {
        let p = qp(expression, "a + *b?");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_infix_with_left_and_right_hand_prefix_and_postfix_operator() {
        let p = qp(expression, "*a? + *b?");
        assert_eq!(unwrap_progress(p).extent(), (0, 9))
    }

    #[test]
    fn expr_multiple_prefix_operator() {
        let p = qp(expression, "&*a");
        assert_eq!(unwrap_progress(p).extent(), (0, 3))
    }

    #[test]
    fn expr_try_operator() {
        let p = qp(expression, "foo?");
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_box() {
        let p = qp(expression, "box foo");
        assert_eq!(unwrap_progress(p).extent(), (0, 7))
    }

    #[test]
    fn expr_byte_string() {
        let p = qp(expression, r#"b"hello""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 8))
    }

    #[test]
    fn expr_byte_string_escape() {
        let p = qp(expression, r#"b"he\"llo""#);
        assert_eq!(unwrap_progress(p).extent(), (0, 10))
    }

    #[test]
    fn expr_byte_string_raw() {
        let p = qp(expression, r###"br#"hello"#"###);
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_byte() {
        let p = qp(expression, r#"b'a'"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 4))
    }

    #[test]
    fn expr_byte_escape() {
        let p = qp(expression, r#"b'\''"#);
        assert_eq!(unwrap_progress(p).extent(), (0, 5))
    }

    #[test]
    fn expr_disambiguation() {
        let p = qp(expression, "<Foo as Bar>::quux");
        assert_eq!(unwrap_progress(p).extent(), (0, 18))
    }

    #[test]
    fn expr_disambiguation_without_disambiguation() {
        let p = qp(expression, "<Foo>::quux");
        assert_eq!(unwrap_progress(p).extent(), (0, 11))
    }

    #[test]
    fn expr_followed_by_block_disallows_struct_literal() {
        let p = qp(expr_followed_by_block, "a {}");
        let (e, b) = unwrap_progress(p);
        assert_eq!(e.extent(), (0, 1));
        assert_eq!(b.extent, (2, 4));
    }

    #[test]
    fn expr_followed_by_block_with_compound_condition() {
        let p = qp(expr_followed_by_block, "a && b {}");
        let (e, b) = unwrap_progress(p);
        assert_eq!(e.extent(), (0, 6));
        assert_eq!(b.extent, (7, 9));
    }

    #[test]
    fn expr_followed_by_block_with_parenthesized_struct_literal() {
        let p = qp(expr_followed_by_block, "(a {}) {}");
        let (e, b) = unwrap_progress(p);
        assert_eq!(e.extent(), (0, 6));
        let p = e.value.into_parenthetical().unwrap();
        assert!(p.expression.is_value());
        assert_eq!(b.extent, (7, 9));
    }

    #[test]
    fn expr_followed_by_block_with_open_ended_range() {
        let p = qp(expr_followed_by_block, "0.. {}");
        let (e, b) = unwrap_progress(p);
        assert_eq!(e.extent(), (0, 3));
        assert!(e.is_range());
        assert_eq!(b.extent, (4, 6));
    }

    #[test]
    fn expr_followed_by_block_with_range_with_curly_start() {
        let p = qp(expr_followed_by_block, "{0}.. {}");
        let (e, b) = unwrap_progress(p);
        assert_eq!(e.extent(), (0, 5));
        assert!(e.is_range());
        assert_eq!(b.extent, (6, 8));
    }

    #[test]
    fn match_arm_with_alternate() {
        let p = qp(match_arm, "a | b => 1");
        assert_eq!(unwrap_progress(p).extent, (0, 10))
    }

    #[test]
    fn match_arm_with_guard() {
        let p = qp(match_arm, "a if a > 2 => 1");
        assert_eq!(unwrap_progress(p).extent, (0, 15))
    }

    #[test]
    fn match_arm_with_attribute() {
        let p = qp(match_arm, "#[cfg(cool)] _ => 1");
        assert_eq!(unwrap_progress(p).extent, (0, 19))
    }
}
