use std::io::{Result, Write};

use lang_c::ast::*;
use lang_c::span::Node;

use crate::write_base::*;

impl<T: WriteLine> WriteLine for Node<T> {
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        self.node.write_line(indent, write)
    }
}

impl<T: WriteString> WriteString for Node<T> {
    fn write_string(&self) -> String {
        self.node.write_string()
    }
}

/// 由于C语言的各个要素从大到小是按树形排布的
/// 因此下面我将使用广度优先搜索的方式来遍历这棵树
/// 并以此方式来排列代码

/// 第1层

impl WriteLine for TranslationUnit {
    /// VERY BIG HINT: You should start by understanding the [`writeln!`](https://doc.rust-lang.org/std/macro.writeln.html) macro.
    fn write_line(&self, indent: usize, write: &mut dyn Write) -> Result<()> {
        let nodes = &self.0;
        for node in nodes {
            // node has type Node<ExternalDeclaration>
            writeln!(write, "{}", node.write_string());
        }

        Ok(())
    }
}

/// 第2层

impl WriteString for ExternalDeclaration {
    fn write_string(&self) -> String {
        let result = match self {
            Self::Declaration(decl) => decl.write_string(),
            Self::StaticAssert(sa) => sa.write_string(),
            Self::FunctionDefinition(r#fn) => r#fn.write_string(),
        };
        format!("{}\n", result)
    }
}

/// 第3层

impl WriteString for Declaration {
    fn write_string(&self) -> String {
        format!(
            "{} {}\n",
            self.specifiers
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
            self.declarators
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
        )
    }
}

impl WriteString for StaticAssert {
    fn write_string(&self) -> String {
        format!(
            "_Static_assert({}, {})",
            self.expression.write_string(),
            self.message.write_string()
        )
    }
}

impl WriteString for FunctionDefinition {
    fn write_string(&self) -> String {
        let specifiers = &self.specifiers;
        let declarator = &self.declarator;
        let declarations = &self.declarations;
        let statement = &self.statement;

        format!(
            "{} {} ({}) {{\n{}\n}}",
            specifiers
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
            declarator.write_string(),
            declarations
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
            statement.write_string()
        )
    }
}

/// 第4层

impl WriteString for DeclarationSpecifier {
    fn write_string(&self) -> String {
        match self {
            Self::StorageClass(sc) => sc.write_string(),
            Self::TypeSpecifier(ts) => ts.write_string(),
            Self::TypeQualifier(tq) => tq.write_string(),
            Self::Function(r#fn) => r#fn.write_string(),
            Self::Alignment(align) => align.write_string(),
            Self::Extension(exts) => exts.iter().map(|ext| ext.write_string()).collect(),
        }
    }
}

impl WriteString for InitDeclarator {
    fn write_string(&self) -> String {
        let declarator = &self.declarator;
        let initializer = &self.initializer;

        format!(
            "{} {}",
            declarator.write_string(),
            initializer.write_string()
        )
    }
}

impl WriteString for Expression {
    fn write_string(&self) -> String {
        match self {
            Self::Identifier(id) => id.write_string(),
            Self::Constant(c) => c.write_string(),
            Self::StringLiteral(sl) => sl.write_string(),
            Self::GenericSelection(gs) => gs.write_string(),
            Self::Member(m) => m.write_string(),
            Self::Call(c) => c.write_string(),
            Self::CompoundLiteral(cl) => cl.write_string(),
            Self::SizeOfTy(sot) => sot.write_string(),
            Self::SizeOfVal(sov) => sov.write_string(),
            Self::AlignOf(ao) => ao.write_string(),
            Self::UnaryOperator(uo) => uo.write_string(),
            Self::Cast(c) => c.write_string(),
            Self::BinaryOperator(bo) => bo.write_string(),
            Self::Conditional(c) => c.write_string(),
            Self::Comma(c) => c.iter().map(|item| item.write_string()).collect(),
            Self::OffsetOf(oo) => oo.write_string(),
            Self::VaArg(va) => va.write_string(),
            Self::Statement(s) => s.write_string(),
        }
    }
}

impl WriteString for StringLiteral {
    fn write_string(&self) -> String {
        self.join("")
    }
}

impl WriteString for Declarator {
    fn write_string(&self) -> String {
        let kind = &self.kind;
        let derived = &self.derived;
        let extensions = &self.extensions;

        let kind_str = kind.write_string();
        let derived_str: String = derived.iter().map(|item| item.write_string()).collect();
        let extensions_str: String = extensions.iter().map(|item| item.write_string()).collect();

        format!("{} {} {}", kind_str, derived_str, extensions_str)
    }
}

impl WriteString for Statement {
    fn write_string(&self) -> String {
        let result = match self {
            Self::Labeled(labeled) => labeled.write_string(),
            Self::Compound(compound) => compound.iter().map(|item| item.write_string()).collect(),
            Self::Expression(exp) => exp.clone().unwrap().write_string(),
            Self::If(r#if) => r#if.write_string(),
            Self::Switch(switch) => switch.write_string(),
            Self::While(r#while) => r#while.write_string(),
            Self::DoWhile(dowhile) => dowhile.write_string(),
            Self::For(r#for) => r#for.write_string(),
            Self::Goto(goto) => goto.write_string(),
            Self::Continue => "continue".to_string(),
            Self::Break => "break".to_string(),
            Self::Return(r#return) => format!("return {}", r#return.write_string()),
            Self::Asm(asm) => asm.write_string(),
        };

        format!("{};\n", result)
    }
}

/// 第5层

impl WriteString for StorageClassSpecifier {
    fn write_string(&self) -> String {
        match self {
            Self::Typedef => "typedef".to_string(),
            Self::Extern => "extern".to_string(),
            Self::Static => "static".to_string(),
            Self::ThreadLocal => "_Thread_local".to_string(),
            Self::Auto => "auto".to_string(),
            Self::Register => "register".to_string(),
        }
    }
}

impl WriteString for TypeSpecifier {
    fn write_string(&self) -> String {
        match self {
            Self::Void => "void".to_string(),
            Self::Char => "char".to_string(),
            Self::Short => "short".to_string(),
            Self::Int => "int".to_string(),
            Self::Long => "long".to_string(),
            Self::Float => "float".to_string(),
            Self::Double => "double".to_string(),
            Self::Signed => "signed".to_string(),
            Self::Unsigned => "unsigned".to_string(),
            Self::Bool => "_Bool".to_string(),
            Self::Complex => "_Complex".to_string(),
            Self::Atomic(atomic) => atomic.write_string(),
            Self::Struct(r#struct) => r#struct.write_string(),
            Self::Enum(r#enum) => r#enum.write_string(),
            Self::TypedefName(typedef_name) => typedef_name.write_string(),
            Self::TypeOf(r#typeof) => r#typeof.write_string(),
            Self::TS18661Float(ts18661_float) => ts18661_float.write_string(),
        }
    }
}

impl WriteString for TypeQualifier {
    fn write_string(&self) -> String {
        match self {
            Self::Const => "const".to_string(),
            Self::Restrict => "restrict".to_string(),
            Self::Volatile => "volatile".to_string(),
            Self::Nonnull => "_Nonnull".to_string(),
            Self::NullUnspecified => "_Null_unspecified".to_string(),
            Self::Nullable => "_Nullable".to_string(),
            Self::Atomic => "_Atomic".to_string(),
        }
    }
}

impl WriteString for FunctionSpecifier {
    fn write_string(&self) -> String {
        match self {
            Self::Inline => "inline".to_string(),
            Self::Noreturn => "_Noreturn".to_string(),
        }
    }
}

impl WriteString for AlignmentSpecifier {
    fn write_string(&self) -> String {
        match self {
            Self::Type(node) => node.write_string(),
            Self::Constant(node) => node.write_string(),
        }
    }
}

impl WriteString for Extension {
    fn write_string(&self) -> String {
        match self {
            Self::Attribute(node) => node.write_string(),
            Self::AsmLabel(node) => node.write_string(),
            Self::AvailabilityAttribute(node) => node.write_string(),
        }
    }
}

impl WriteString for Initializer {
    fn write_string(&self) -> String {
        match self {
            Self::Expression(node) => node.write_string(),
            Self::List(nodes) => nodes.iter().map(|node| node.write_string()).collect(),
        }
    }
}

impl WriteString for Identifier {
    fn write_string(&self) -> String {
        self.name.clone()
    }
}

impl WriteString for Constant {
    fn write_string(&self) -> String {
        match self {
            Self::Integer(i) => i.number.to_string(),
            Self::Float(f) => f.number.to_string(),
            Self::Character(c) => c.clone(),
        }
    }
}

impl WriteString for GenericSelection {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for MemberExpression {
    fn write_string(&self) -> String {
        let op = &self.operator;
        let expr = &self.expression;
        let id = &self.identifier;

        format!(
            "{} {} {}",
            op.write_string(),
            expr.write_string(),
            id.write_string()
        )
    }
}

impl WriteString for CallExpression {
    fn write_string(&self) -> String {
        let callee = &self.callee;
        let args = &self.arguments;

        format!(
            "{}({})",
            callee.write_string(),
            args.iter()
                .map(|item| item.write_string())
                .collect::<String>()
        )
    }
}

impl WriteString for CompoundLiteral {
    fn write_string(&self) -> String {
        let type_name = &self.type_name;
        let init_list = &self.initializer_list;

        format!(
            "({}){{{}}}",
            type_name.write_string(),
            init_list
                .iter()
                .map(|item| item.write_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl WriteString for SizeOfTy {
    fn write_string(&self) -> String {
        format!("sizeof({})", self.0.write_string())
    }
}

impl WriteString for SizeOfVal {
    fn write_string(&self) -> String {
        format!("sizeof({})", self.0.write_string())
    }
}

impl WriteString for AlignOf {
    fn write_string(&self) -> String {
        format!("_Alignof({})", self.0.write_string())
    }
}

impl WriteString for UnaryOperatorExpression {
    fn write_string(&self) -> String {
        format!(
            "{}{}",
            self.operator.write_string(),
            self.operand.write_string()
        )
    }
}

impl WriteString for CastExpression {
    fn write_string(&self) -> String {
        format!(
            "({}){}",
            self.type_name.write_string(),
            self.expression.write_string()
        )
    }
}

impl WriteString for BinaryOperatorExpression {
    fn write_string(&self) -> String {
        let lhs = &self.lhs;
        let op = &self.operator;
        let rhs = &self.rhs;

        format!(
            "{} {} {}",
            lhs.write_string(),
            op.write_string(),
            rhs.write_string()
        )
    }
}

impl WriteString for ConditionalExpression {
    fn write_string(&self) -> String {
        let c = &self.condition;
        let t = &self.then_expression;
        let e = &self.else_expression;

        format!(
            "{} ? {} : {}",
            c.write_string(),
            t.write_string(),
            e.write_string()
        )
    }
}

impl WriteString for OffsetOfExpression {
    fn write_string(&self) -> String {
        let t = &self.type_name;
        let d = &self.designator;

        format!("offsetof({}, {})", t.write_string(), d.write_string())
    }
}

impl WriteString for VaArgExpression {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for DeclaratorKind {
    fn write_string(&self) -> String {
        match self {
            Self::Abstract => "<abstract declarator>".to_string(),
            Self::Identifier(id) => id.node.name.clone(),
            Self::Declarator(decl) => decl.write_string(),
        }
    }
}

impl WriteString for DerivedDeclarator {
    fn write_string(&self) -> String {
        match self {
            Self::Pointer(nodes) => nodes.iter().map(|item| item.write_string()).collect(),
            Self::Array(node) => node.write_string(),
            Self::Function(node) => node.write_string(),
            Self::KRFunction(nodes) => nodes.iter().map(|item| item.write_string()).collect(),
            Self::Block(nodes) => nodes.iter().map(|item| item.write_string()).collect(),
        }
    }
}

impl WriteString for LabeledStatement {
    fn write_string(&self) -> String {
        format!(
            "{} : {}",
            self.label.write_string(),
            self.statement.write_string()
        )
    }
}

impl WriteString for BlockItem {
    fn write_string(&self) -> String {
        match self {
            Self::Declaration(node) => node.write_string(),
            Self::StaticAssert(node) => node.write_string(),
            Self::Statement(node) => node.write_string(),
        }
    }
}

impl WriteString for IfStatement {
    fn write_string(&self) -> String {
        format!(
            "
if ({}) {{
    {}
}} else {{
    {}
}}
            ",
            self.condition.write_string(),
            self.then_statement.write_string(),
            self.else_statement.write_string()
        )
    }
}

impl WriteString for SwitchStatement {
    fn write_string(&self) -> String {
        format!(
            "switch ({}) {{\n{}\n}}",
            self.expression.write_string(),
            self.statement.write_string()
        )
    }
}

impl WriteString for WhileStatement {
    fn write_string(&self) -> String {
        format!(
            "while ({}) {{\n{}\n}}",
            self.expression.write_string(),
            self.statement.write_string()
        )
    }
}

impl WriteString for DoWhileStatement {
    fn write_string(&self) -> String {
        format!(
            "do {{\n{}\n}} while ({})",
            self.statement.write_string(),
            self.expression.write_string(),
        )
    }
}

impl WriteString for ForStatement {
    fn write_string(&self) -> String {
        format!(
            "for ({}; {}; {}) {{\n{}\n}}",
            self.initializer.write_string(),
            self.condition.write_string(),
            self.step.write_string(),
            self.statement.write_string(),
        )
    }
}

impl WriteString for AsmStatement {
    fn write_string(&self) -> String {
        todo!()
    }
}

/// 第6层

impl WriteString for TypeName {
    fn write_string(&self) -> String {
        format!(
            "{} {}",
            self.specifiers
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
            if self.declarator.is_some() {
                self.declarator.write_string()
            } else {
                "".to_string()
            }
        )
    }
}

impl WriteString for StructType {
    fn write_string(&self) -> String {
        format!(
            "{} {} {}",
            self.kind.write_string(),
            if self.identifier.is_some() {
                self.identifier.as_ref().unwrap().write_string()
            } else {
                "".to_string()
            },
            if self.declarations.is_some() {
                self.declarations
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|item| item.write_string())
                    .collect::<String>()
            } else {
                "".to_string()
            }
        )
    }
}

impl WriteString for EnumType {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for TypeOf {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for TS18661FloatType {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for Attribute {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for AvailabilityAttribute {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for InitializerListItem {
    fn write_string(&self) -> String {
        format!(
            "{} {}",
            self.designation
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
            self.initializer.write_string()
        )
    }
}

impl WriteString for GenericAssociation {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for MemberOperator {
    fn write_string(&self) -> String {
        match self {
            Self::Direct => ".".to_string(),
            Self::Indirect => "->".to_string(),
        }
    }
}

impl WriteString for UnaryOperator {
    fn write_string(&self) -> String {
        match self {
            Self::PostIncrement => "++".to_string(),
            Self::PostDecrement => "--".to_string(),
            Self::PreIncrement => "++".to_string(),
            Self::PreDecrement => "--".to_string(),
            Self::Address => "&".to_string(),
            Self::Indirection => "*".to_string(),
            Self::Plus => "+".to_string(),
            Self::Minus => "-".to_string(),
            Self::Complement => "~".to_string(),
            Self::Negate => "!".to_string(),
        }
    }
}

impl WriteString for BinaryOperator {
    fn write_string(&self) -> String {
        match self {
            Self::Index => "[]".to_string(),
            Self::Multiply => "*".to_string(),
            Self::Divide => "/".to_string(),
            Self::Modulo => "%".to_string(),
            Self::Plus => "+".to_string(),
            Self::Minus => "-".to_string(),
            Self::ShiftLeft => "<<".to_string(),
            Self::ShiftRight => ">>".to_string(),
            Self::Less => "<".to_string(),
            Self::Greater => ">".to_string(),
            Self::LessOrEqual => "<=".to_string(),
            Self::GreaterOrEqual => ">=".to_string(),
            Self::Equals => "==".to_string(),
            Self::NotEquals => "!=".to_string(),
            Self::BitwiseAnd => "&".to_string(),
            Self::BitwiseXor => "^".to_string(),
            Self::BitwiseOr => "|".to_string(),
            Self::LogicalAnd => "&&".to_string(),
            Self::LogicalOr => "||".to_string(),
            Self::Assign => "=".to_string(),
            Self::AssignMultiply => "*=".to_string(),
            Self::AssignDivide => "/=".to_string(),
            Self::AssignModulo => "%=".to_string(),
            Self::AssignPlus => "+=".to_string(),
            Self::AssignMinus => "-=".to_string(),
            Self::AssignShiftLeft => "<<=".to_string(),
            Self::AssignShiftRight => ">>=".to_string(),
            Self::AssignBitwiseAnd => "&=".to_string(),
            Self::AssignBitwiseXor => "^=".to_string(),
            Self::AssignBitwiseOr => "|=".to_string(),
        }
    }
}

impl WriteString for OffsetDesignator {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for PointerQualifier {
    fn write_string(&self) -> String {
        match self {
            Self::TypeQualifier(node) => node.write_string(),
            Self::Extension(nodes) => nodes.iter().map(|item| item.write_string()).collect(),
        }
    }
}

impl WriteString for ArrayDeclarator {
    fn write_string(&self) -> String {
        format!(
            "{} {}",
            self.qualifiers
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
            self.size.write_string()
        )
    }
}

impl WriteString for FunctionDeclarator {
    fn write_string(&self) -> String {
        format!(
            "{} {}",
            self.parameters
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
            self.ellipsis.write_string()
        )
    }
}

impl WriteString for Label {
    fn write_string(&self) -> String {
        match self {
            Self::Identifier(node) => node.write_string(),
            Self::Case(node) => node.write_string(),
            Self::CaseRange(node) => node.write_string(),
            Self::Default => "".to_string(),
        }
    }
}

impl WriteString for ForInitializer {
    fn write_string(&self) -> String {
        match self {
            Self::Empty => "".to_string(),
            Self::Expression(node) => node.write_string(),
            Self::Declaration(node) => node.write_string(),
            Self::StaticAssert(node) => node.write_string(),
        }
    }
}

impl WriteString for GnuExtendedAsmStatement {
    fn write_string(&self) -> String {
        todo!()
    }
}

/// 第7层

impl WriteString for SpecifierQualifier {
    fn write_string(&self) -> String {
        match self {
            Self::TypeSpecifier(node) => node.write_string(),
            Self::TypeQualifier(node) => node.write_string(),
            Self::Extension(nodes) => nodes.iter().map(|item| item.write_string()).collect(),
        }
    }
}

impl WriteString for StructKind {
    fn write_string(&self) -> String {
        match self {
            Self::Struct => "struct".to_string(),
            Self::Union => "union".to_string(),
        }
    }
}

impl WriteString for StructDeclaration {
    fn write_string(&self) -> String {
        match self {
            Self::Field(node) => node.write_string(),
            Self::StaticAssert(node) => node.write_string(),
        }
    }
}

impl WriteString for Enumerator {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for AvailabilityClause {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for Designator {
    fn write_string(&self) -> String {
        match self {
            Self::Index(node) => node.write_string(),
            Self::Member(node) => node.write_string(),
            Self::Range(node) => node.write_string(),
        }
    }
}

impl WriteString for GenericAssociationType {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for OffsetMember {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for ParameterDeclaration {
    fn write_string(&self) -> String {
        format!(
            "{} {} {}",
            self.specifiers
                .iter()
                .map(|item| item.write_string())
                .collect::<String>(),
            self.declarator.write_string(),
            self.extensions
                .iter()
                .map(|item| item.write_string())
                .collect::<String>()
        )
    }
}

impl WriteString for Ellipsis {
    fn write_string(&self) -> String {
        match self {
            Self::None => "".to_string(),
            Self::Some => "...".to_string(),
        }
    }
}

impl WriteString for CaseRange {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for GnuAsmOperand {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for ArraySize {
    fn write_string(&self) -> String {
        match self {
            Self::Unknown => "".to_string(),
            Self::VariableUnknown => "".to_string(),
            Self::VariableExpression(node) => node.write_string(),
            Self::StaticExpression(node) => node.write_string(),
        }
    }
}

/// 第8层

impl WriteString for StructField {
    fn write_string(&self) -> String {
        let specifiers_strs: String = self
            .specifiers
            .iter()
            .map(|item| item.write_string())
            .collect();
        let declarators_strs: String = self
            .declarators
            .iter()
            .map(|item| item.write_string())
            .collect();
        specifiers_strs + &declarators_strs
    }
}

impl WriteString for AvailabilityVersion {
    fn write_string(&self) -> String {
        todo!()
    }
}

impl WriteString for RangeDesignator {
    fn write_string(&self) -> String {
        let from_str = self.from.write_string();
        let to_str = self.to.write_string();

        format!("[{from_str} ... {to_str}]")
    }
}

/// 第9层

impl WriteString for StructDeclarator {
    fn write_string(&self) -> String {
        let declarator_str = if let Some(decl) = &self.declarator {
            decl.write_string()
        } else {
            "".to_string()
        };

        let bit_width_str = if let Some(bw) = &self.bit_width {
            bw.write_string()
        } else {
            "".to_string()
        };

        format!("{declarator_str} : {bit_width_str}")
    }
}
