use crate::{ast, ir, symbols, types, unique_ids};

#[derive(Debug, Clone, PartialEq)]
pub enum ExpResult {
    PlainOperand(ir::IrValue),
    DereferencedPointer(ir::IrValue),
    SubObject(String, i64),
}

fn create_tmp(t: types::Type) -> String {
    let name = unique_ids::make_temporary();
    symbols::add_automatic_var(name.clone(), t);
    name
}

fn convert_op(op: ast::UnaryOperator) -> ir::UnaryOperator {
    match op {
        ast::UnaryOperator::Negate => ir::UnaryOperator::Negate,
    }
}

fn get_ptr_scale(t: types::Type) -> usize {
    match t {
        types::Type::Pointer(referenced) => types::get_type_size(*referenced),
        _ => panic!(
            "Internal error: tried to get scale of non-pointer type: {:#?}",
            t
        ),
    }
}

fn conver_binop(op: ast::BinaryOperator) -> ir::BinaryOperator {
    match op {
        ast::BinaryOperator::Add => ir::BinaryOperator::Add,
        ast::BinaryOperator::Subtract => ir::BinaryOperator::Subtract,
        ast::BinaryOperator::Multiply => ir::BinaryOperator::Multiply,
        ast::BinaryOperator::Divide => ir::BinaryOperator::Divide,
        ast::BinaryOperator::Equal => ir::BinaryOperator::Equal,
        ast::BinaryOperator::NotEqual => ir::BinaryOperator::NotEqual,
        ast::BinaryOperator::GreaterOrEqual => ir::BinaryOperator::GreaterOrEqual,
        ast::BinaryOperator::GreaterThan => ir::BinaryOperator::GreaterThan,
        ast::BinaryOperator::LessOrEqual => ir::BinaryOperator::LessOrEqual,
        ast::BinaryOperator::LessThan => ir::BinaryOperator::LessThan,
    }
}

fn emit_ir_for_exp(typed_exp: ast::TypedExp) -> (Vec<ir::Instruction>, ExpResult) {
    match typed_exp.e {
        ast::TypedInnerExp::Constant(c) => {
            (vec![], ExpResult::PlainOperand(ir::IrValue::Constant(c)))
        }
        ast::TypedInnerExp::Var(v) => (vec![], ExpResult::PlainOperand(ir::IrValue::Var(v))),
        ast::TypedInnerExp::Unary(op, inner) => emit_unary_expression(typed_exp.t, op, *inner),
        ast::TypedInnerExp::Binary(ast::BinaryOperator::Add, e1, e2)
            if types::is_pointer(typed_exp.t.clone()) =>
        {
            emit_pointer_addition(typed_exp.t, *e1, *e2)
        }
        ast::TypedInnerExp::Binary(op, e1, e2) => emit_binary_expression(typed_exp.t, op, *e1, *e2),
        _ => panic!(),
    }
}

fn emit_ir_and_convert(typed_exp: ast::TypedExp) -> (Vec<ir::Instruction>, ir::IrValue) {
    let (mut instructions, result) = emit_ir_for_exp(typed_exp.clone());
    match result {
        ExpResult::PlainOperand(o) => (instructions, o),
        ExpResult::DereferencedPointer(ptr) => {
            let dst = ir::IrValue::Var(create_tmp(typed_exp.t));
            instructions.push(ir::Instruction::Load {
                src_ptr: ptr,
                dst: dst.clone(),
            });
            (instructions, dst)
        }
        ExpResult::SubObject(base, offset) => {
            let dst = ir::IrValue::Var(create_tmp(typed_exp.t));
            instructions.push(ir::Instruction::CopyFromOffset {
                src: base,
                offset,
                dst: dst.clone(),
            });
            (instructions, dst)
        }
    }
}

fn emit_unary_expression(
    t: types::Type,
    op: ast::UnaryOperator,
    inner: ast::TypedExp,
) -> (Vec<ir::Instruction>, ExpResult) {
    let (mut eval_inner, v) = emit_ir_and_convert(inner);
    let dst_name = create_tmp(t);
    let dst = ir::IrValue::Var(dst_name);
    let ir_op = convert_op(op);
    eval_inner.push(ir::Instruction::Unary {
        op: ir_op,
        src: v,
        dst: dst.clone(),
    });
    (eval_inner, ExpResult::PlainOperand(dst))
}

fn emit_binary_expression(
    t: types::Type,
    op: ast::BinaryOperator,
    e1: ast::TypedExp,
    e2: ast::TypedExp,
) -> (Vec<ir::Instruction>, ExpResult) {
    let (mut eval_v1, v1) = emit_ir_and_convert(e1);
    let (mut eval_v2, v2) = emit_ir_and_convert(e2);
    let dst_name = create_tmp(t);
    let dst = ir::IrValue::Var(dst_name);
    let ir_op = conver_binop(op);
    let mut instructions = vec![];
    instructions.append(&mut eval_v1);
    instructions.append(&mut eval_v2);
    instructions.push(ir::Instruction::Binary {
        op: ir_op,
        src1: v1,
        src2: v2,
        dst: dst.clone(),
    });
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_pointer_addition(
    t: types::Type,
    e1: ast::TypedExp,
    e2: ast::TypedExp,
) -> (Vec<ir::Instruction>, ExpResult) {
    let (mut eval_v1, v1) = emit_ir_and_convert(e1.clone());
    let (mut eval_v2, v2) = emit_ir_and_convert(e2.clone());
    let dst_name = create_tmp(t.clone());
    let dst = ir::IrValue::Var(dst_name);
    let (ptr, index) = if t == e1.t { (v1, v2) } else { (v2, v1) };
    let scale = get_ptr_scale(t) as i64;
    let mut instructions = vec![];
    instructions.append(&mut eval_v1);
    instructions.append(&mut eval_v2);
    instructions.push(ir::Instruction::AddrPtr {
        ptr,
        index,
        scale,
        dst: dst.clone(),
    });
    (instructions, ExpResult::PlainOperand(dst))
}

fn emit_ir_for_statement(
    stmt: ast::Statement<ast::TypedInitializer, ast::TypedExp>,
) -> Vec<ir::Instruction> {
    match stmt {
        ast::Statement::Return(e) => {
            let (mut eval_exp, v) = match e {
                Some(_e) => {
                    let (instrs, result) = emit_ir_and_convert(_e);
                    (instrs, Some(result))
                }
                None => (vec![], None),
            };
            eval_exp.push(ir::Instruction::Return(v));
            eval_exp
        }
        ast::Statement::Expression(e) => {
            let (eval_exp, _) = emit_ir_for_exp(e);
            eval_exp
        }
        ast::Statement::Null => vec![],
        _ => panic!(),
    }
}
