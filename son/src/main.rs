use sti::traits::CopyIt;
use sti::reader::Reader;
use sti::vec::Vec;
use sti::keyed::KVec;


struct Tokenizer<'a> {
    reader: Reader<'a, u8>,
}

impl<'a> Tokenizer<'a> {
    fn skip_ws(&mut self) {
        self.reader.consume_while(u8::is_ascii_whitespace);
    }

    fn peek(&self) -> Option<u8> {
        self.reader.get(0).copied()
    }

    fn expect_int(&mut self) -> i32 {
        let (bytes, ok) = self.reader.consume_while_slice(u8::is_ascii_digit);
        if !ok {
            panic!("syntax error: expected integer at {}", self.reader.offset());
        }
        let bytes = core::str::from_utf8(bytes).unwrap();
        i32::from_str_radix(bytes, 10).unwrap_or_else(|_|
            panic!("synatx error: integer too large"))
    }

    fn expect(&mut self, string: &str) {
        self.skip_ws();
        if !self.reader.starts_with(string.as_bytes()) {
            panic!("syntax error: expected {:?} at {}", string, self.reader.offset());
        }
        self.reader.consume(string.len());
    }
}

struct Parser<'a> {
    tok: Tokenizer<'a>,
    son: &'a mut Son,
}

impl<'a> Parser<'a> {
    fn parse_file(son: &mut Son, input: &[u8]) -> NodeId {
        let mut this = Parser {
            tok: Tokenizer {
                reader: Reader::new(input),
            },
            son,
        };

        let start = this.son.new_start_node();
        this.son.set_start(start);

        this.tok.expect("return");
        let value = this.parse_expr();
        this.tok.expect(";");
        this.son.new_return_node(start, value);

        return start;
    }

    fn parse_expr(&mut self) -> NodeId {
        self.tok.skip_ws();

        let Some(at) = self.tok.peek() else {
            panic!("unexpected eof");
        };

        if at.is_ascii_digit() {
            let value = self.tok.expect_int();
            self.son.new_const_node(value)
        }
        else {
            panic!("unexpected {:?} at {}", at as char, self.tok.reader.offset());
        }
    }
}


sti::define_key!(u32, NodeId);

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
}


fn main() {
    let mut son = Son::new();

    let start = Parser::parse_file(&mut son, br#"
        return 1;
    "#);

    dbg!(&son.nodes);
}

