use sti::traits::CopyIt;
use sti::reader::Reader;
use sti::vec::Vec;
use sti::keyed::KVec;


#[derive(Clone, Copy, Debug)]
struct Token {
    kind: TokenKind,
    begin: u32,
    end: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TokenKind {
    Eof,
    Int(i32),
    Semicolon,
    Add,
    Sub,
    Mul,
    Div,
    Return,
}

impl TokenKind {
    fn prec_left(self) -> Option<u32> {
        Some(match self {
            TokenKind::Add => 600,
            TokenKind::Sub => 600,
            TokenKind::Mul => 800,
            TokenKind::Div => 800,
            _ => return None,
        })
    }

    fn prec_right(self) -> u32 {
        match self {
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

    fn next(&mut self) -> Token {
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
                    "return" => TokenKind::Return,
                    _ => todo!()
                }
            }

            b'+' => TokenKind::Add,
            b'-' => TokenKind::Sub,
            b'*' => TokenKind::Mul,
            b'/' => TokenKind::Div,

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
}

impl<'a> Parser<'a> {
    fn parse_file(son: &mut Son, input: &[u8]) {
        let mut this = Parser {
            tok: Tokenizer {
                reader: Reader::new(input),
            },
            son,
        };

        let start = this.son.new_start_node();
        this.son.set_start(start);

        this.tok.expect(TokenKind::Return);
        let value = this.parse_expr(0);
        this.tok.expect(TokenKind::Semicolon);
        this.son.new_return_node(start, value);
    }

    fn parse_expr(&mut self, prec: u32) -> NodeId {
        let mut result = self.parse_leading_expr();
        loop {
            let save = self.tok.save();
            let at = self.tok.next();
            let at_prec = at.kind.prec_left();
            if at.kind.prec_left().filter(|p| *p >= prec).is_none() {
                self.tok.restore(save);
                return result;
            }

            let rhs = self.parse_expr(at.kind.prec_right());
            result = match at.kind {
                TokenKind::Add => self.son.new_add_node(result, rhs),
                TokenKind::Sub => self.son.new_sub_node(result, rhs),
                TokenKind::Mul => self.son.new_mul_node(result, rhs),
                TokenKind::Div => self.son.new_div_node(result, rhs),
                _ => unreachable!()
            };
        }
    }

    fn parse_leading_expr(&mut self) -> NodeId {
        let at = self.tok.next();
        match at.kind {
            TokenKind::Int(v) => {
                self.son.new_const_node(v)
            }

            TokenKind::Sub => {
                let v = self.parse_expr(PREC_PREFIX);
                self.son.new_neg_node(v)
            }

            _ => {
                panic!("syntax error: unexpected {:?} at {}", at.kind, at.begin);
            }
        }
    }
}


sti::define_key!(u32, NodeId, opt: OptNodeId);

#[derive(Debug)]
struct Node {
    kind: NodeKind,
    ins: Vec<NodeId>,
    outs: Vec<NodeId>,
}

#[derive(Debug)]
enum NodeKind {
    Start,
    Return,
    Const(i32),
    Neg,
    Add,
    Sub,
    Mul,
    Div,
}

struct Son {
    nodes: KVec<NodeId, Node>,
    start: NodeId,
}

impl Son {
    fn new() -> Self {
        Self {
            nodes: KVec::new(),
            start: NodeId::MAX,
        }
    }

    fn set_start(&mut self, id: NodeId) {
        self.start = id;
    }

    fn new_node(&mut self, kind: NodeKind, ins: &[NodeId]) -> NodeId {
        let id = self.nodes.push(Node {
            kind,
            ins: ins.into(),
            outs: Vec::new(),
        });

        for n in ins.copy_it() {
            self.nodes[n].outs.push(id);
        }

        return id;
    }

    fn new_start_node(&mut self) -> NodeId {
        self.new_node(NodeKind::Start, &[])
    }

    fn new_return_node(&mut self, ctrl: NodeId, value: NodeId) -> NodeId {
        self.new_node(NodeKind::Return, &[ctrl, value])
    }

    fn new_const_node(&mut self, value: i32) -> NodeId {
        self.new_node(NodeKind::Const(value), &[self.start])
    }

    fn new_neg_node(&mut self, value: NodeId) -> NodeId {
        self.new_node(NodeKind::Neg, &[self.start, value])
    }

    fn new_add_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::Add, &[self.start, lhs, rhs])
    }

    fn new_sub_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::Sub, &[self.start, lhs, rhs])
    }

    fn new_mul_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::Mul, &[self.start, lhs, rhs])
    }

    fn new_div_node(&mut self, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.new_node(NodeKind::Div, &[self.start, lhs, rhs])
    }
}


fn main() {
    let mut son = Son::new();

    Parser::parse_file(&mut son, br#"
        return 1;
    "#);
    dbg!(&son.nodes);

    Parser::parse_file(&mut son, br#"
        return 1 + 2 * 3 + -5;
    "#);
    dbg!(&son.nodes);
}

