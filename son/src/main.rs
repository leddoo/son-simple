use sti::traits::CopyIt;
use sti::reader::Reader;
use sti::vec::Vec;
use sti::keyed::KVec;


#[derive(Clone, Copy, Debug)]
struct Token<'a> {
    kind: TokenKind<'a>,
    begin: u32,
    #[allow(dead_code)]
    end: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TokenKind<'a> {
    Eof,
    Int(i32),
    Ident(&'a str),
    Semicolon,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    CmpEq,
    CmpNe,
    CmpLe,
    CmpLt,
    CmpGe,
    CmpGt,
    Not,
    LParen,
    RParen,
    LCurly,
    RCurly,
    KwReturn,
    KwVar,
}

impl<'a> TokenKind<'a> {
    fn prec_left(self) -> Option<u32> {
        Some(match self {
            TokenKind::CmpEq => 400,
            TokenKind::CmpNe => 400,
            TokenKind::CmpLe => 400,
            TokenKind::CmpLt => 400,
            TokenKind::CmpGe => 400,
            TokenKind::CmpGt => 400,
            TokenKind::Add => 600,
            TokenKind::Sub => 600,
            TokenKind::Mul => 800,
            TokenKind::Div => 800,
            _ => return None,
        })
    }

    fn prec_right(self) -> u32 {
        match self {
            TokenKind::CmpEq => 401,
            TokenKind::CmpNe => 401,
            TokenKind::CmpLe => 401,
            TokenKind::CmpLt => 401,
            TokenKind::CmpGe => 401,
            TokenKind::CmpGt => 401,
            TokenKind::Add => 601,
            TokenKind::Sub => 601,
            TokenKind::Mul => 801,
            TokenKind::Div => 801,
            _ => unreachable!()
        }
    }
}

const PREC_PREFIX: u32 = 900;


struct Tokenizer<'a> {
    reader: Reader<'a, u8>,
}

impl<'a> Tokenizer<'a> {
    fn skip_ws(&mut self) {
        self.reader.consume_while(u8::is_ascii_whitespace);
    }

    fn next(&mut self) -> Token<'a> {
        self.skip_ws();

        let begin = self.reader.offset();
        let Some(at) = self.reader.next() else {
            let begin = begin as u32;
            return Token { kind: TokenKind::Eof, begin, end: begin };
        };

        let kind = match at {
            b'0'..=b'9' => {
                let bytes = self.reader.consume_while_slice_from(begin, u8::is_ascii_digit).0;
                let str = core::str::from_utf8(bytes).unwrap();
                let value = i32::from_str_radix(str, 10).unwrap_or_else(|_|
                    panic!("syntax error: integer too large"));
                TokenKind::Int(value)
            }

            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let bytes = self.reader.consume_while_slice_from(begin, |at|
                    at.is_ascii_alphanumeric() || *at == b'_').0;
                let str = core::str::from_utf8(bytes).unwrap();
                match str {
                    "return" => TokenKind::KwReturn,
                    "var" => TokenKind::KwVar,
                    _ => TokenKind::Ident(str),
                }
            }

            b'+' => TokenKind::Add,
            b'-' => TokenKind::Sub,
            b'*' => TokenKind::Mul,
            b'/' => TokenKind::Div,

            b'=' => {
                if self.reader.consume_if_eq(&b'=') {
                    TokenKind::CmpEq
                }
                else { TokenKind::Eq }
            }

            b'!' => {
                if self.reader.consume_if_eq(&b'=') {
                    TokenKind::CmpNe
                }
                else { TokenKind::Not }
            }

            b'<' => {
                if self.reader.consume_if_eq(&b'=') {
                    TokenKind::CmpLe
                }
                else { TokenKind::CmpLt }
            }

            b'>' => {
                if self.reader.consume_if_eq(&b'=') {
                    TokenKind::CmpGe
                }
                else { TokenKind::CmpGt }
            }

            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LCurly,
            b'}' => TokenKind::RCurly,

            b';' => TokenKind::Semicolon,

            _ => {
                panic!("syntax error: unexpected {:?} at {}", at as char, begin);
            }
        };

        Token {
            kind,
            begin: begin as u32,
            end: self.reader.offset() as u32,
        }
    }

    fn expect(&mut self, kind: TokenKind) {
        let at = self.next();
        if at.kind != kind {
            panic!("syntax error: expected {:?} at {}", kind, at.begin);
        }
    }

    fn expect_ident(&mut self) -> &'a str {
        let at = self.next();
        let TokenKind::Ident(result) = at.kind else {
            panic!("syntax error: expected \"identifier\" at {}", at.begin);
        };
        return result;
    }

    fn save(&self) -> usize {
        return self.reader.offset();
    }

    fn restore(&mut self, save: usize) {
        self.reader.set_offset(save);
    }
}

struct Parser<'a> {
    tok: Tokenizer<'a>,
    son: &'a mut Son,
    locals: Vec<Scope<'a>>,
}

#[derive(Debug)]
struct Scope<'a> {
    name: &'a str,
    value: NodeId,
}

impl<'a> Parser<'a> {
    fn parse_file(son: &mut Son, input: &[u8]) {
        let mut this = Parser {
            tok: Tokenizer {
                reader: Reader::new(input),
            },
            son,
            locals: Vec::new(),
        };

        let start = this.son.new_start_node(&[Type::Control, Type::I32(I32Type::Bottom)][..]).peephole(this.son);

        let entry = this.son.new_proj_node(start, 0).peephole(this.son);
        this.son.set_ctrl(entry);

        this.locals.push(Scope {
            name: "arg",
            value: this.son.new_proj_node(start, 1).peephole(this.son).keep_alive(this.son),
        });

        this.parse_block(TokenKind::Eof);

        this.son.clear_ctrl();
        assert_eq!(this.locals.len(), 1);
        this.locals[0].value.remove_keep_alive(this.son);
    }

    fn parse_block(&mut self, end: TokenKind) {
        let locals_save = self.locals.len();
        loop {
            let save = self.tok.save();
            let at = self.tok.next();
            if at.kind == end {
                break;
            }
            self.tok.restore(save);

            self.parse_stmt();
        }
        for i in locals_save..self.locals.len() {
            self.locals[i].value.remove_keep_alive(self.son);
        }
        self.locals.truncate(locals_save);
    }

    fn parse_stmt(&mut self) {
        let at = self.tok.next();
        match at.kind {
            TokenKind::Semicolon => (),

            TokenKind::KwVar => {
                let name = self.tok.expect_ident();
                self.tok.expect(TokenKind::Eq);
                let value = self.parse_expr(0);
                value.keep_alive(self.son);
                self.locals.push(Scope { name, value });
                self.tok.expect(TokenKind::Semicolon);
            }

            TokenKind::KwReturn => {
                let value = self.parse_expr(0);
                self.son.new_return_node(self.son.ctrl, value).peephole(self.son);
                self.tok.expect(TokenKind::Semicolon);
            }

            TokenKind::LCurly => {
                self.parse_block(TokenKind::RCurly);
            }

            TokenKind::Ident(name) => {
                let idx = self.local(name);
                self.tok.expect(TokenKind::Eq);
                let value = self.parse_expr(0);
                value.keep_alive(self.son);
                let old_value = core::mem::replace(&mut self.locals[idx].value, value);
                old_value.remove_keep_alive(self.son);
                self.tok.expect(TokenKind::Semicolon);
            }

            _ => {
                panic!("syntax error: unexpected {:?} at {} while parsing statement", at.kind, at.begin);
            }
        }
    }

    fn parse_expr(&mut self, prec: u32) -> NodeId {
        let mut result = self.parse_leading_expr();
        loop {
            let save = self.tok.save();
            let at = self.tok.next();
            if at.kind.prec_left().filter(|p| *p >= prec).is_none() {
                self.tok.restore(save);
                return result;
            }

            let rhs = self.parse_expr(at.kind.prec_right());

            result = match at.kind {
                TokenKind::CmpEq => self.son.new_cmp_eq_node(result, rhs),
                TokenKind::CmpNe => { let eq = self.son.new_cmp_eq_node(result, rhs).peephole(self.son); self.son.new_not_node(eq) },
                TokenKind::CmpLe => self.son.new_cmp_le_node(result, rhs),
                TokenKind::CmpLt => self.son.new_cmp_lt_node(result, rhs),
                TokenKind::CmpGe => { let lt = self.son.new_cmp_lt_node(result, rhs).peephole(self.son); self.son.new_not_node(lt) },
                TokenKind::CmpGt => { let le = self.son.new_cmp_le_node(result, rhs).peephole(self.son); self.son.new_not_node(le) },
                TokenKind::Add => self.son.new_add_node(result, rhs),
                TokenKind::Sub => self.son.new_sub_node(result, rhs),
                TokenKind::Mul => self.son.new_mul_node(result, rhs),
                TokenKind::Div => self.son.new_div_node(result, rhs),
                _ => unreachable!()
            }.peephole(self.son);
        }
    }

    fn parse_leading_expr(&mut self) -> NodeId {
        let at = self.tok.next();
        match at.kind {
            TokenKind::Int(v) => {
                self.son.new_const_node(v).peephole(self.son)
            }

            TokenKind::Sub => {
                let v = self.parse_expr(PREC_PREFIX);
                self.son.new_neg_node(v).peephole(self.son)
            }

            TokenKind::Not => {
                let v = self.parse_expr(PREC_PREFIX);
                self.son.new_not_node(v).peephole(self.son)
            }

            TokenKind::Ident(name) => {
                let idx = self.local(name);
                self.locals[idx].value
            }

            TokenKind::LParen => {
                let result = self.parse_expr(0);
                self.tok.expect(TokenKind::RParen);
                result
            }

            _ => {
                panic!("syntax error: unexpected {:?} at {} while parsing expression", at.kind, at.begin);
            }
        }
    }

    fn local(&self, name: &str) -> usize {
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.name == name {
                return i;
            }
        }
        panic!("error: unknown local {:?}", name);
    }
}


sti::define_key!(u32, NodeId);

impl NodeId {
    const NONE: NodeId = NodeId(0);

    fn is_none(self) -> bool {
        self == Self::NONE
    }
}

#[derive(Debug)]
struct Node {
    kind: NodeKind,
    ty: Type,
    ins: Vec<NodeId>,
    outs: Vec<NodeId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Dead,
    Top,
    Bottom,
    Control,
    I32(I32Type),
    Tuple(Vec<Type>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum I32Type {
    Top,
    Const(i32),
    Bottom,
}

impl Type {
    #[inline]
    fn is_dead(&self) -> bool {
        matches!(self, Type::Dead)
    }

    fn is_constant(&self) -> bool {
        match self {
            Type::Dead => unreachable!(),
            Type::Top => true,
            Type::Bottom => false,
            Type::Control => false,
            Type::I32(t) => match t {
                I32Type::Top => true,
                I32Type::Const(_) => true,
                I32Type::Bottom => false,
            },
            Type::Tuple(_) => false,
        }
    }

    fn as_i32(&self) -> I32Type {
        match self {
            Type::Top => I32Type::Top,
            Type::I32(t) => *t,
            _ => unreachable!()
        }
    }

    fn meet(&self, other: &Type) -> Type {
        assert!(!self.is_dead());
        assert!(!other.is_dead());

        if self == other {
            return self.clone();
        }

        match (self, other) {
            (Type::I32(t1), Type::I32(t2)) => Type::I32(t1.meet(*t2)),

            (Type::Tuple(_), Type::Tuple(_)) => todo!(),

            _ => Type::Bottom
        }
    }
}

impl I32Type {
    fn meet(self, other: Self) -> Self {
        if self == other {
            return self;
        }
        match (self, other) {
            (I32Type::Top, t2) => t2,
            (t1, I32Type::Top) => t1,
            _ => I32Type::Bottom,
        }
    }
}

impl From<i32> for Type {
    fn from(value: i32) -> Self {
        Self::I32(I32Type::Const(value))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum NodeKind {
    Dead,
    Start,
    Return,
    Proj(usize),
    Const,
    Neg,
    Not,
    CmpEq,
    CmpLe,
    CmpLt,
    Add,
    Sub,
    Mul,
    Div,
}

impl NodeId {
    #[inline]
    fn kind(self, son: &Son) -> NodeKind {
        son.nodes[self].kind
    }

    #[inline]
    fn ty(self, son: &Son) -> &Type {
        &son.nodes[self].ty
    }

    #[inline]
    fn ins(self, son: &Son) -> &[NodeId] {
        &son.nodes[self].ins
    }

    #[inline]
    fn outs(self, son: &Son) -> &[NodeId] {
        &son.nodes[self].outs
    }

    #[inline]
    fn is_unused(self, son: &Son) -> bool {
        self.outs(son).is_empty()
    }

    #[inline]
    fn is_dead(self, son: &Son) -> bool {
        if self.kind(son) == NodeKind::Dead {
            assert!(self.ty(son).is_dead());
            assert!(self.ins(son).is_empty());
            assert!(self.outs(son).is_empty());
            true
        }
        else {
            assert!(!self.ty(son).is_dead());
            false
        }
    }

    fn keep_alive(self, son: &mut Son) -> Self {
        self.add_out(NodeId::NONE, son);
        self
    }

    fn remove_keep_alive(self, son: &mut Son) {
        if self.remove_out(NodeId::NONE, son) {
            self.kill(son);
        }
    }

    fn set_in(self, i: usize, new_in: NodeId, son: &mut Son) {
        assert!(!self.is_dead(son));

        let old_in = self.ins(son)[i];
        if new_in == old_in {
            return;
        }

        if !new_in.is_none() {
            new_in.add_out(self, son);
        }

        if !old_in.is_none() && old_in.remove_out(self, son) {
            old_in.kill(son);
        }

        let this = &mut son.nodes[self];
        this.ins[i] = new_in;
    }

    fn add_out(self, new_out: NodeId, son: &mut Son) {
        assert!(!self.is_dead(son));
        let this = &mut son.nodes[self];
        this.outs.push(new_out);
    }

    fn remove_out(self, old_out: NodeId, son: &mut Son) -> bool {
        assert!(!self.is_dead(son));
        let this = &mut son.nodes[self];
        let index = this.outs.iter().position(|o| *o == old_out)
            .expect("error: {old_out:?} is not in {self:?}'s outs");
        this.outs.remove_swap(index);
        return this.outs.is_empty();
    }

    fn kill(self, son: &mut Son) {
        assert!(!self.is_dead(son));
        assert!(self.is_unused(son));

        for i in 0..self.ins(son).len() {
            self.set_in(i, NodeId::NONE, son);
        }
        let this = &mut son.nodes[self];
        this.kind = NodeKind::Dead;
        this.ty = Type::Dead;
        this.ins.clear();

        assert!(self.is_dead(son));
    }

    fn peephole(self, son: &mut Son) -> NodeId {
        assert!(!self.is_dead(son));

        let ty = self.compute(son);

        if ty.is_constant() && self.kind(son) != NodeKind::Const {
            let result = son.new_const_node(ty);
            self.kill(son);
            return result;
        }
        else {
            return self.idealize(son);
        }
    }

    fn compute(self, son: &mut Son) -> Type {
        assert!(!self.is_dead(son));

        let this = &son.nodes[self];

        let ty = match this.kind {
            NodeKind::Dead => unreachable!(),

            NodeKind::Start => this.ty.clone(),

            NodeKind::Return => {
                Type::Tuple(sti::vec![
                    this.ins[0].ty(son).clone(),
                    this.ins[1].ty(son).clone(),
                ])
            }

            NodeKind::Proj(index) => {
                let Type::Tuple(tys) = this.ins[0].ty(son) else { unreachable!() };
                tys[index].clone()
            }

            NodeKind::Const => this.ty.clone(),

            NodeKind::Neg => Type::I32({
                let ty = this.ins[1].ty(son).as_i32();
                if let I32Type::Const(v) = ty {
                    I32Type::Const(v.wrapping_neg())
                }
                else { ty }
            }),

            NodeKind::Not => Type::I32({
                let ty = this.ins[1].ty(son).as_i32();
                if let I32Type::Const(v) = ty {
                    I32Type::Const(if v == 0 { 1 } else { 0 })
                }
                else { ty }
            }),

            NodeKind::CmpEq |
            NodeKind::CmpLe |
            NodeKind::CmpLt => Type::I32({
                let lhs = this.ins[1].ty(son).as_i32();
                let rhs = this.ins[2].ty(son).as_i32();
                if let (I32Type::Const(lhs), I32Type::Const(rhs)) = (lhs, rhs) {
                    let value = match this.kind {
                        NodeKind::CmpEq => lhs == rhs,
                        NodeKind::CmpLe => lhs <= rhs,
                        NodeKind::CmpLt => lhs <  rhs,
                        _ => unreachable!()
                    };
                    I32Type::Const(value as i32)
                }
                else { lhs.meet(rhs) }
            }),

            NodeKind::Add => Type::I32({
                let lhs = this.ins[1].ty(son).as_i32();
                let rhs = this.ins[2].ty(son).as_i32();
                if let (I32Type::Const(lhs), I32Type::Const(rhs)) = (lhs, rhs) {
                    I32Type::Const(lhs.wrapping_add(rhs))
                }
                else { lhs.meet(rhs) }
            }),

            NodeKind::Sub => Type::I32({
                let lhs = this.ins[1].ty(son).as_i32();
                let rhs = this.ins[2].ty(son).as_i32();
                if let (I32Type::Const(lhs), I32Type::Const(rhs)) = (lhs, rhs) {
                    I32Type::Const(lhs.wrapping_sub(rhs))
                }
                else { lhs.meet(rhs) }
            }),

            NodeKind::Mul => Type::I32({
                let lhs = this.ins[1].ty(son).as_i32();
                let rhs = this.ins[2].ty(son).as_i32();
                if let (I32Type::Const(lhs), I32Type::Const(rhs)) = (lhs, rhs) {
                    I32Type::Const(lhs.wrapping_mul(rhs))
                }
                else { lhs.meet(rhs) }
            }),

            NodeKind::Div => {
                let lhs = this.ins[1].ty(son).as_i32();
                let rhs = this.ins[2].ty(son).as_i32();
                if let (I32Type::Const(lhs), I32Type::Const(rhs)) = (lhs, rhs) {
                    if rhs != 0 { Type::I32(I32Type::Const(lhs.wrapping_mul(rhs))) }
                    else        { Type::Top }
                }
                else { Type::I32(lhs.meet(rhs)) }
            }
        };

        son.nodes[self].ty = ty.clone();
        return ty;
    }

    fn idealize(self, son: &mut Son) -> NodeId {
        assert!(!self.is_dead(son));

        self
    }
}

struct Son {
    nodes: KVec<NodeId, Node>,
    ctrl: NodeId
}

impl Son {
    fn new() -> Self {
        let mut this = Self {
            nodes: KVec::new(),
            ctrl: NodeId::MAX,
        };

        let none = this.nodes.push(Node {
            kind: NodeKind::Dead,
            ty: Type::Dead,
            ins: Vec::new(),
            outs: Vec::new(),
        });
        assert_eq!(none, NodeId::NONE);

        return this;
    }

    fn set_ctrl(&mut self, ctrl: NodeId) {
        self.ctrl = ctrl.keep_alive(self);
    }

    fn clear_ctrl(&mut self) {
        core::mem::replace(&mut self.ctrl, NodeId::MAX)
            .remove_keep_alive(self);
    }

    fn new_node(&mut self, kind: NodeKind, ty: Type, ins: &[NodeId]) -> NodeId {
        for n in ins.copy_it() {
            assert!(!n.is_dead(self));
        }

        let id = self.nodes.push(Node {
            kind,
            ty,
            ins: ins.into(),
            outs: Vec::new(),
        });

        for n in ins.copy_it() {
            self.nodes[n].outs.push(id);
        }

        return id;
    }

    fn new_start_node(&mut self, types: impl Into<Vec<Type>>) -> NodeId {
        self.new_node(NodeKind::Start, Type::Tuple(types.into()), &[])
    }

    fn new_return_node(&mut self, ctrl: NodeId, value: NodeId) -> NodeId {
        self.new_node(NodeKind::Return, Type::Bottom, &[ctrl, value])
    }

    fn new_proj_node(&mut self, node: NodeId, index: usize) -> NodeId {
        self.new_node(NodeKind::Proj(index), Type::Bottom, &[node])
    }

    fn new_const_node(&mut self, value: impl Into<Type>) -> NodeId {
        self.new_node(NodeKind::Const, value.into(), &[self.ctrl])
    }

    fn new_neg_node(&mut self, value: NodeId) -> NodeId {
        self.new_node(NodeKind::Neg, Type::Bottom, &[self.ctrl, value])
    }

    fn new_not_node(&mut self, value: NodeId) -> NodeId {
        self.new_node(NodeKind::Not, Type::Bottom, &[self.ctrl, value])
    }

    fn new_cmp_eq_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::CmpEq, Type::Bottom, &[self.ctrl, lhs, rhs])
    }

    fn new_cmp_le_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::CmpLe, Type::Bottom, &[self.ctrl, lhs, rhs])
    }

    fn new_cmp_lt_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::CmpLt, Type::Bottom, &[self.ctrl, lhs, rhs])
    }

    fn new_add_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::Add, Type::Bottom, &[self.ctrl, lhs, rhs])
    }

    fn new_sub_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::Sub, Type::Bottom, &[self.ctrl, lhs, rhs])
    }

    fn new_mul_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::Mul, Type::Bottom, &[self.ctrl, lhs, rhs])
    }

    fn new_div_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::Div, Type::Bottom, &[self.ctrl, lhs, rhs])
    }
}


fn main() {
    let mut son = Son::new();
    Parser::parse_file(&mut son, br#"
        return 1;
    "#);
    dbg!(&son.nodes);

    let mut son = Son::new();
    Parser::parse_file(&mut son, br#"
        return 1 + 2 * 3 + -5;
    "#);
    dbg!(&son.nodes);

    let mut son = Son::new();
    Parser::parse_file(&mut son, br#"
        var a=1;
        var b=2;
        var c=0;
        {
          var b=3;
          c=a+b;
        }
        return c;
    "#);
    dbg!(&son.nodes);

    let mut son = Son::new();
    Parser::parse_file(&mut son, br#"
        var x0=1;
        var y0=2;
        var x1=3;
        var y1=4;
        return (x0-x1)*(x0-x1) + (y0-y1)*(y0-y1);
    "#);
    dbg!(&son.nodes);

    let mut son = Son::new();
    Parser::parse_file(&mut son, br#"
        return 1 + arg + 2;
    "#);
    dbg!(&son.nodes);

    let mut son = Son::new();
    Parser::parse_file(&mut son, br#"
        return arg + (1 + 2);
    "#);
    dbg!(&son.nodes);

    let mut son = Son::new();
    Parser::parse_file(&mut son, br#"
        return (2 >= 3) + (7 != 1) * (1 < 2);
    "#);
    dbg!(&son.nodes);
}

